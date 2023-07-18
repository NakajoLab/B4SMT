package b4processor.modules.lsq

import b4processor.Parameters
import b4processor.structures.memoryAccess.MemoryAccessInfo
import b4processor.utils.operations.{LoadStoreOperation, LoadStoreWidth}
import b4processor.utils.Tag
import chisel3._

/** LSQのエントリ
  *
  * @param params
  *   パラメータ
  */
class LoadStoreQueueEntry(implicit params: Parameters) extends Bundle {

  /** エントリが有効である */
  val valid = Bool()

  /** 命令がリオーダバッファでコミットされたか */
  val readyReorderSign = Bool()

  /** メモリアクセスの情報 */
  val operation = LoadStoreOperation()
  val operationWidth = LoadStoreWidth()

  /** 命令自体を識別するためのタグ(Destination Tag) */
  val destinationTag = new Tag

  /** アドレス値 */
  val address = UInt(64.W)

  val addressOffset = SInt(12.W)

  /** アドレス値が有効である */
  val addressValid = Bool()

  val addressTag = new Tag

  /** ストアに使用するデータが格納されるタグ(SourceRegister2 Tag) */
  val storeDataTag = new Tag

  /** ストアデータ */
  val storeData = UInt(64.W)

  /** ストアデータが有効である */
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
    storeDataValid: Bool
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
