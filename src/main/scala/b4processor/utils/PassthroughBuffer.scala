package b4processor.utils

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class PassthroughBuffer[T <: Data](t: T) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(t))
    val output = Irrevocable(t)
  })

  io.output <> PassthroughBuffer(io.input)
}

object PassthroughBuffer extends App {
  ChiselStage.emitSystemVerilogFile(
    new PassthroughBuffer(UInt(4.W)),
    Array.empty,
    Array(
      "--disable-mem-randomization",
      "--disable-reg-randomization",
      "--disable-all-randomization",
    ),
  )

  def apply[T <: Data](input: ReadyValidIO[T]): IrrevocableIO[T] = {
    val t: T = input.bits.cloneType
    val output = Wire(new IrrevocableIO[T](t))

    val buf = Reg(t)
    val bufSet = RegInit(false.B)

    output.valid := false.B
    output.bits := 0.U.asTypeOf(t)
    input.ready := false.B

    when(!bufSet) {
      input.ready := true.B
      when(input.valid) {
        output.valid := true.B
        output.bits := input.bits
        when(!output.ready) {
          buf := input.bits
          bufSet := true.B
        }
      }
    }.otherwise {
      output.valid := true.B
      output.bits := buf
      when(!input.valid && output.ready) {
        buf := 0.U.asTypeOf(t)
        bufSet := false.B
      }
    }

    output
  }
}
