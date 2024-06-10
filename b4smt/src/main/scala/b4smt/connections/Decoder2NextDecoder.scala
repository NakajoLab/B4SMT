package b4smt.connections

import b4smt.Parameters
import b4smt.utils.Tag
import chisel3._

/** デコーダ同士をつなぐ
  *
  * @param params
  *   パラメータ
  */
class Decoder2NextDecoder(implicit params: Parameters) extends Bundle {
  val valid = Bool()
  val destinationTag = new Tag
  val destinationRegister = UInt(5.W)
}
