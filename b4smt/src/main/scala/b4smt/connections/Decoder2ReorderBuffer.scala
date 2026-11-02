package b4smt.connections

import b4smt.Parameters
import b4smt.utils.{ForPext, RVRegister, Tag}
import chisel3._
import chisel3.util._

/** Connects the decoder and reorder buffer
  *
  * @param params
  *   Parameters
  */
class Decoder2ReorderBuffer(implicit params: Parameters) extends Bundle {
  val sources = Vec(3, new SourceRegister())
  val destination = new DestinationRegister()
  val programCounter = Output(UInt(64.W))
  val isDecodeError = Output(Bool())
  // Making the entire bundle Decoupled would incorrectly make source1 and the other fields all Output, so define it here
  val ready = Input(Bool())
  val valid = Output(Bool())

  class SourceRegister extends Bundle {
    val sourceRegister = Output(new RVRegister())
    val matchingTag = Flipped(Valid(new Tag))
    val value = Flipped(Valid(UInt(64.W)))
  }

  class DestinationRegister extends Bundle {
    val destinationRegister = Output(new RVRegister())
    val destinationTag = Input(new Tag)
    val operationInorder = Output(Bool())
  }
}
