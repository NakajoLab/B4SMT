package b4processor.modules.executor

import b4processor.Parameters
import b4processor.connections.ResultType
import b4processor.utils.Tag
import chisel3._

class ExecutionRegisterBypassForTest(implicit val params: Parameters)
    extends Bundle {
  val destinationTag = Output(new Tag)
  val value = Output(SInt(64.W))
  val valid = Output(Bool())
  val resultType = new ResultType.Type()
  val ready = Input(Bool())
}
