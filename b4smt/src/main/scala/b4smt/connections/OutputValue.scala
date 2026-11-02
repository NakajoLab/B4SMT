package b4smt.connections

import b4smt.Parameters
import b4smt.utils.Tag
import chisel3._

class OutputValue(implicit params: Parameters) extends Bundle {

  /** Value */
  val value = Output(UInt(64.W))

  /// An error occurred
  val isError = Bool()

  /** Corresponding tag */
  val tag = Output(new Tag)
}
