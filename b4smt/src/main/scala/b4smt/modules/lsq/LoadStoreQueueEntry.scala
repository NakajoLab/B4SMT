package b4smt.modules.lsq

import b4smt.Parameters
import b4smt.structures.memoryAccess.MemoryAccessInfo
import b4smt.utils.operations.{LoadStoreOperation, LoadStoreWidth}
import b4smt.utils.Tag
import chisel3._

/** LSQ entry
  *
  * @param params
  *   Parameters
  */
class LoadStoreQueueEntry(implicit params: Parameters) extends Bundle {

  /** The entry is valid */
  val valid = Bool()

  /** Whether the instruction has been committed in the reorder buffer */
  val readyReorderSign = Bool()

  /** Memory access information */
  val operation = LoadStoreOperation()
  val operationWidth = LoadStoreWidth()

  /** Tag identifying the instruction itself (Destination Tag) */
  val destinationTag = new Tag

  /** Address value */
  val address = UInt(64.W)

  val addressOffset = SInt(12.W)

  /** The address value is valid */
  val addressValid = Bool()

  val addressTag = new Tag

  /** Tag holding the data to store (SourceRegister2 Tag) */
  val storeDataTag = new Tag

  /** Store data */
  val storeData = UInt(64.W)

  /** The store data is valid */
  val storeDataValid = Bool()
}

object LoadStoreQueueEntry {
  def validEntry(
    operation: LoadStoreOperation.Type,
    operationWidth: LoadStoreWidth.Type,
    destinationTag: Tag,
    address: UInt,
    addressValid: Bool,
    addressOffset: SInt,
    addressTag: Tag,
    storeDataTag: Tag,
    storeData: UInt,
    storeDataValid: Bool,
  )(implicit params: Parameters): LoadStoreQueueEntry = {
    val entry = LoadStoreQueueEntry.default
    entry.valid := true.B

    entry.operation := operation
    entry.operationWidth := operationWidth

    entry.destinationTag := destinationTag
    entry.address := address
    entry.addressValid := addressValid
    entry.addressOffset := addressOffset
    entry.addressTag := addressTag

    entry.storeDataTag := storeDataTag
    entry.storeData := storeData
    entry.storeDataValid := storeDataValid

    entry
  }

  def default(implicit params: Parameters): LoadStoreQueueEntry = {
    val entry = Wire(new LoadStoreQueueEntry)
    entry.valid := false.B
    entry.readyReorderSign := false.B

    entry.operation := DontCare
    entry.operationWidth := DontCare

    entry.destinationTag := Tag(0, 0)
    entry.addressTag := Tag(0, 0)
    entry.address := 0.U
    entry.addressValid := false.B
    entry.addressOffset := 0.S

    entry.storeDataTag := Tag(0, 0)
    entry.storeData := 0.U
    entry.storeDataValid := false.B

    entry
  }
}
