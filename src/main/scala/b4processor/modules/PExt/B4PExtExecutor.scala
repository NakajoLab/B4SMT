package b4processor.modules.PExt

import b4processor.Parameters
import b4processor.connections.{OutputValue, ReservationStation2PExtExecutor}
import chisel3._
import chisel3.util
import chisel3.util.Decoupled
import circt.stage.ChiselStage

class B4PExtExecutor(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(new ReservationStation2PExtExecutor()))
    val output = Decoupled(new OutputValue)
//    val input = Input(new Bundle {
//      val oeration = new PExtensionOperation.Type()
//      val rs1 = UInt(64.W)
//      val rs2 = UInt(64.W)
//      val rs3 = UInt(64.W)
//      val rd = UInt(64.W)
//      val imm = UInt(6.W)
//    })
//    val output = Output(new Bundle {
//      val value = UInt(64.W)
//      val overflow = Bool()
//    })
  })

  val PextMod = Module(new PExtExecutor)
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
}

object B4PExtExecutor extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(
    new B4PExtExecutor(),
    firtoolOpts = Array(
      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb",
    ),
  )
}
