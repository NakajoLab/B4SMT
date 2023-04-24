package b4processor.modules.decoder

import chisel3._

object SourceTagFrom extends ChiselEnum {
  val ReorderBuffer = Value
  val BeforeDecoder = Value
}
