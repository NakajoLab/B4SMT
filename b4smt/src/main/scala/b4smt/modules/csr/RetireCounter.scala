package b4smt.modules.csr

import b4smt.Parameters
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class RetireCounter(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val retireInCycle =
      Input(UInt(log2Up(params.maxRegisterFileCommitCount + 1).W))
    val count = Output(UInt(64.W))
  })

  val c = RegInit(0.U(64.W))
  c := c + io.retireInCycle
  io.count := c
}

object RetireCounter extends App {
  implicit val params: b4smt.Parameters = Parameters()
  ChiselStage.emitSystemVerilogFile(new RetireCounter)
}
