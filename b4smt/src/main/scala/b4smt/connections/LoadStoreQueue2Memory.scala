package b4smt.connections

import b4smt.Parameters
import b4smt.utils.operations.{LoadStoreOperation, LoadStoreWidth}
import b4smt.utils.Tag
import chisel3._

/** LSQとメモリをつなぐ
  */
class LoadStoreQueue2Memory(implicit params: Parameters) extends Bundle {
  val address = UInt(64.W)
  val tag = new Tag()
  val data = UInt(64.W)
  val operation = LoadStoreOperation()
  val operationWidth = LoadStoreWidth()
}
