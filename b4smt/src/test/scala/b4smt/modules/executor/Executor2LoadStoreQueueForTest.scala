package b4smt.modules.executor

import b4smt.Parameters
import chisel3._

class Executor2LoadStoreQueueForTest(implicit val params: Parameters)
    extends Bundle {
  val destinationTag = UInt(params.tagWidth.W)
  val value = SInt(64.W)
  val valid = Bool()
  val programCounter = SInt(64.W)
}
