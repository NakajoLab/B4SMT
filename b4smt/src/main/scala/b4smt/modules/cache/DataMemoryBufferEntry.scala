package b4smt.modules.cache

import b4smt.Parameters
import b4smt.utils.operations.{LoadStoreOperation, LoadStoreWidth}
import b4smt.utils.Tag
import chisel3._

class DataMemoryBufferEntry(implicit params: Parameters) extends Bundle {

  /** Address value */
  val address = UInt(64.W)

  /** Tag identifying the instruction (Destination Tag) */
  val tag = new Tag

  /** Store data */
  val data = UInt(64.W)

  /** Memory access information */
  val operation = LoadStoreOperation()
  val operationWidth = LoadStoreWidth()
}

object DataMemoryBufferEntry {
  def validEntry(
    address: UInt,
    tag: Tag,
    data: UInt,
    operation: LoadStoreOperation.Type,
    operationWidth: LoadStoreWidth.Type,
  )(implicit params: Parameters): DataMemoryBufferEntry = {
    val entry = DataMemoryBufferEntry.default
    entry.address := address
    entry.tag := tag
    entry.data := data
    entry.operation := operation
    entry.operationWidth := operationWidth

    entry
  }

  def default(implicit params: Parameters): DataMemoryBufferEntry = {
    val entry = Wire(new DataMemoryBufferEntry)
    entry := DontCare

    entry
  }
}
