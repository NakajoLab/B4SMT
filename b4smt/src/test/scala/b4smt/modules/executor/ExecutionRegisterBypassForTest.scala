package b4smt.modules.executor

import b4smt.Parameters

import b4smt.utils.Tag
import chisel3._

class ExecutionRegisterBypassForTest(implicit val params: Parameters)
    extends Bundle {
  val destinationTag = Output(new Tag)
  val value = Output(SInt(64.W))
  val valid = Output(Bool())
  val ready = Input(Bool())
}
