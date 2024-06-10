package b4smt.connections

import b4smt.Parameters
import b4smt.utils.Tag
import chisel3._

class OutputValue(implicit params: Parameters) extends Bundle {

  /** 値 */
  val value = Output(UInt(64.W))

  /// エラーだった
  val isError = Bool()

  /** 対応するタグ */
  val tag = Output(new Tag)
}
