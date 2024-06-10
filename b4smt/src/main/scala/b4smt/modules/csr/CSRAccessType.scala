package b4smt.modules.csr

import chisel3._

object CSRAccessType extends ChiselEnum {
  val ReadWrite, ReadSet, ReadClear = Value
}
