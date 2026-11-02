package b4smt.connections

import b4smt.Parameters
import chisel3._
import chisel3.util._

/** Send data from the ALU to the LSQ
  *
  * @param params
  *   Parameters
  */

class Executor2LoadStoreQueue(implicit params: Parameters) extends Bundle {
  val destinationTag = UInt(params.tagWidth.W)
  val value = UInt(64.W)
  val valid = Bool()
}
