package b4processor.connections

import b4processor.Parameters
import b4processor.utils.Tag
import chisel3._

class OutputValue(implicit params: Parameters) extends Bundle {

  /** 値 */
  val value = Output(UInt(64.W))

  /// エラーだった
  val isError = Bool()

  /** 対応するタグ */
  val tag = Output(new Tag)
}
