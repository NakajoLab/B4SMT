package b4smt.modules.executor

import b4smt.Parameters
import b4smt.connections.{OutputValue, ReservationStation2MulDivExecutor}
import b4smt.utils.operations.MulDivOperation
import b4smt.utils.operations.MulDivOperation.SignType
import chisel3._
import chisel3.util._

class MulDivExecutor(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val reservationStation =
      Flipped(Decoupled(new ReservationStation2MulDivExecutor()))
    val out = Decoupled(new OutputValue)
  })

  val mul = Module(new Multiplyer(32))
  val div = Module(new DivRem(32))

  mul.io <> DontCare
  div.io <> DontCare
  io.out.valid := false.B
  io.out.bits <> DontCare
  io.reservationStation.ready := false.B

  val waitingInput :: executing :: waitingOutput :: Nil = Enum(3)
  val state = RegInit(waitingInput)

  val operationReg = Reg(new MulDivOperation.Type())
  val operation = WireDefault(operationReg)

  val outputReg = Reg(UInt(64.W))
  val outputValid = Reg(Bool())

  switch(state) {
    is(waitingInput) {
      io.reservationStation.ready := true.B
      operation := io.reservationStation.bits.operation
      when(io.reservationStation.valid) {
        operation := io.reservationStation.bits.operation
        operationReg := io.reservationStation.bits.operation
        state := executing
      }

      when(MulDivOperation.isMul(operation)) {
        mul.io.in.valid := io.reservationStation.valid
        mul.io.in.bits.a := io.reservationStation.bits.value1
        mul.io.in.bits.b := io.reservationStation.bits.value2
        mul.io.in.bits.signType := MulDivOperation.signType(operation)
        mul.io.out.ready := true.B
      } otherwise {
        div.io.in.valid := io.reservationStation.valid
        div.io.in.bits.dividend := io.reservationStation.bits.value1
        div.io.in.bits.divisor := io.reservationStation.bits.value2
        div.io.in.bits.signed := MulDivOperation.signType(
          operation,
        ) === SignType.Signed
        div.io.out.ready := true.B
      }
    }
    is(executing) {}
  }
}

class MulIO(inputWidth: Int) extends Bundle {
  val in = Flipped(Decoupled(new Bundle {
    val a = UInt(inputWidth.W)
    val b = UInt(inputWidth.W)
    val signType = SignType.Type()
  }))
  val out = Decoupled(UInt((inputWidth * 2).W))
}

class MulBaseModule(inputWidth: Int) extends Module {
  val io = IO(new MulIO(inputWidth))
}

class Multiplyer(inputWidth: Int) extends MulBaseModule(inputWidth) {
  import SignType._
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits := MuxLookup(io.in.bits.signType, 0.U)(
    Seq(
      Unsigned -> io.in.bits.a * io.in.bits.b,
      Signed -> (io.in.bits.a.asSInt * io.in.bits.b.asSInt).asUInt,
      SignedUnsigned -> (io.in.bits.a.asSInt * io.in.bits.b).asUInt,
    ),
  )
}
class DivRemIO(inputWidth: Int) extends Bundle {
  val in = Flipped(Decoupled(new Bundle {
    val dividend = UInt(inputWidth.W)
    val divisor = UInt(inputWidth.W)
    val signed = Bool()
  }))
  val out = Decoupled(new Bundle {
    val quotient = UInt(inputWidth.W)
    val remainder = UInt(inputWidth.W)
  })
}

class DivRemBaseModule(inputWidth: Int) extends Module {
  val io = IO(new DivRemIO(inputWidth))
}

class DivRem(inputWidth: Int) extends DivRemBaseModule(inputWidth) {
  io.out.bits.quotient := 0.U
  io.out.bits.remainder := 0.U
  io.out.valid := false.B
  io.in.ready := false.B

  private val waitingInput :: changeSignIn :: executing :: changeSignOut :: waitingOutput :: Nil =
    Enum(5)
  private val state = RegInit(waitingInput)

  private val dividend = Reg(UInt((inputWidth * 2).W))
  private val divisor = Reg(UInt(inputWidth.W))
  private val invertQuot = Reg(Bool())
  private val invertRem = Reg(Bool())
  private val counter = Reg(UInt(log2Ceil(inputWidth).W))
  private val signed = Reg(Bool())

  switch(state) {
    is(waitingInput) {
      io.in.ready := true.B
      when(io.in.valid) {
        state := Mux(io.in.bits.signed, changeSignIn, executing)
        signed := io.in.bits.signed
        dividend := io.in.bits.dividend
        divisor := io.in.bits.divisor
        counter := (inputWidth - 1).U
      }
    }
    is(changeSignIn) {
      invertQuot := dividend(inputWidth - 1) ^ divisor(inputWidth - 1)
      invertRem := divisor(inputWidth - 1)
      when(dividend(inputWidth - 1)) {
        dividend := -dividend(inputWidth - 1, 0)
      }
      when(divisor(inputWidth - 1)) {
        divisor := -divisor
      }
      state := executing
    }
    is(executing) {
      when(counter === 0.U) {
        state := Mux(signed, changeSignOut, waitingOutput)
      }
      val cmpVal = dividend(inputWidth * 2 - 2, inputWidth - 1)
      when(cmpVal >= divisor) {
        dividend := Cat(cmpVal - divisor, dividend(inputWidth - 2, 0), 1.U(1.W))
      } otherwise {
        dividend := Cat(dividend, 0.U(1.W))
      }
      counter := counter - 1.U
    }
    is(changeSignOut) {
      dividend := Cat(
        Mux(
          invertRem,
          -dividend(inputWidth * 2 - 1, inputWidth),
          dividend(inputWidth * 2 - 1, inputWidth),
        ),
        Mux(
          invertQuot,
          -dividend(inputWidth - 1, 0),
          dividend(inputWidth - 1, 0),
        ),
      )
      state := waitingOutput
    }
    is(waitingOutput) {
      io.out.valid := true.B
      io.out.bits.quotient := dividend(inputWidth - 1, 0)
      io.out.bits.remainder := dividend(inputWidth * 2 - 1, inputWidth)
      when(io.out.ready) {
        state := waitingInput
      }
    }
  }
}
