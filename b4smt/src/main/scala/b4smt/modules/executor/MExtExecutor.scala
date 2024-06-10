package b4smt.modules.executor

import b4smt.Parameters
import b4smt.connections.OutputValue
import b4smt.utils.operations.MulDivOperation
import chisel3._
import chisel3.util._

class MExtExecutor(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(new Bundle {
      val operation = MulDivOperation.Type()
      val rs1 = UInt(64.W)
      val rs2 = UInt(64.W)
    }))
    val output = Decoupled(new OutputValue())
  })

  val waitingInput :: executing :: waitingOutput :: Nil = Enum(3)
  val state = RegInit(waitingInput)

  val rs1Reg = Reg(UInt(64.W))
  val rs2Reg = Reg(UInt(64.W))
  val opReg = Reg(MulDivOperation.Type())
  val outAcc = Reg(UInt(64.W))

  io.input.ready := state === waitingInput

  when(state === waitingInput) {
    when(io.input.valid) {
      state := executing
      rs1Reg := io.input.bits.rs1
      rs2Reg := io.input.bits.rs2
      opReg := io.input.bits.operation
      outAcc := 0.U
    }
  }

  when(state === executing) {
    io.output.bits.value := rs1Reg + rs2Reg
    switch(opReg) {
      is(MulDivOperation.Mul) {
        outAcc := (rs1Reg * rs2Reg)(63, 0)
      }
      is(MulDivOperation.Mulh) {
        outAcc := (rs1Reg.asSInt * rs2Reg.asSInt)(127, 64)
      }
      is(MulDivOperation.Mulhu) {
        outAcc := (rs1Reg * rs2Reg)(127, 64)
      }
      is(MulDivOperation.Mulhsu) {
        outAcc := (rs1Reg.asSInt * rs2Reg)(127, 64)
      }
      is(MulDivOperation.Div) {
        outAcc := rs1Reg / rs2Reg
      }
    }
  }

}
