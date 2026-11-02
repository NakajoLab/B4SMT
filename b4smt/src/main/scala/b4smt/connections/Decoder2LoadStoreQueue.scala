package b4smt.connections

import b4smt.Parameters
import b4smt.utils.operations.{LoadStoreOperation, LoadStoreWidth}
import b4smt.utils.Tag
import chisel3._

/** Connects the decoder and LSQ
  *
  * @param params
  *   Parameters
  */
class Decoder2LoadStoreQueue(implicit params: Parameters) extends Bundle {

  /** Memory access information */
  val operation = LoadStoreOperation()
  val operationWidth = LoadStoreWidth()

  val destinationTag = new Tag

  val addressTag = new Tag

  /** The address value is valid */
  val address = UInt(64.W)

  /** The address value is valid */
  val addressOffset = SInt(12.W)

  /** The address value is valid */
  val addressValid = Bool()

  /** Tag holding the data to store (SourceRegister2 Tag) */
  val storeDataTag = new Tag

  /** Store data */
  val storeData = UInt(64.W)

  /** Whether the store data value is valid */
  val storeDataValid = Bool()
}
