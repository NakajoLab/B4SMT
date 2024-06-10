package b4smt.modules.executor

import b4smt.Parameters
import b4smt.connections.{OutputValue, ReservationStation2PExtExecutor}
import b4smt_pext.PExtExecutor
import chisel3._
import chisel3.util.Decoupled
import circt.stage.ChiselStage

class B4PExtExecutor(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(new ReservationStation2PExtExecutor()))
    val output = Decoupled(new OutputValue)
    val status = Output(UInt(params.threads.W))
  })

  private val PextMod = Module(new PExtExecutor)
  PextMod.io.input.rs1 := io.input.bits.value1
  PextMod.io.input.rs2 := io.input.bits.value2
  PextMod.io.input.rs3 := io.input.bits.value3
  PextMod.io.input.imm := io.input.bits.value3
  PextMod.io.input.rd := io.input.bits.value3
  PextMod.io.input.oeration := io.input.bits.operation
  io.output.bits.value := PextMod.io.output.value
  io.output.bits.tag := io.input.bits.destinationTag
  io.output.bits.isError := false.B
  io.output.valid := io.input.valid
  io.input.ready := io.output.ready

  io.status := 0.U
  when(io.output.ready && io.output.valid) {
    io.status := 1.U << io.output.bits.tag.threadId
  }
}

object B4PExtExecutor extends App {
  implicit val params: b4smt.Parameters = Parameters()
  ChiselStage.emitSystemVerilogFile(
    new B4PExtExecutor(),
    firtoolOpts = Array(
      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb",
    ),
  )
}
