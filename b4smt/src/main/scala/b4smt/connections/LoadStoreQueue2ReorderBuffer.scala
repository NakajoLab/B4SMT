package b4smt.connections

import b4smt.Parameters
import b4smt.utils.Tag
import chisel3._
import chisel3.util._

/** LSQとリオーダバッファをつなぐ
  */
class LoadStoreQueue2ReorderBuffer(implicit params: Parameters) extends Bundle {
  val destinationTag = new Tag
}
