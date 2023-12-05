package b4processor.modules.reorderbuffer

import b4processor.Parameters
import b4processor.utils.RVRegister
import b4processor.utils.RVRegister.AddRegConstructor
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor

// Entry = {
//  destinationRegister: RVRegister,
//  programCounter: uint64,
//  operationType: Normal(OoO) | Inorder | Canceled,
//  value: None | Ok {value:uint64} | Err {reason:uint64}
// }

class ReorderBufferEntry(implicit params: Parameters) extends Bundle {

  /** デスティネーションレジスタ */
  val destinationRegister = new RVRegister()

  /** 命令の処理が完了した（コミットできる） */
  val valueReady = Bool()

  /** 通常時：実行結果の値 例外時：エラーの種類 */
  val value = UInt(64.W)

  /** プログラムカウンタ */
  val programCounter = UInt(64.W)

  /** インオーダ発行の命令 */
  val operationInorder = Bool()

  /** isError */
  val isError = Bool()

  /** キャンセルされた */
  val canceled = Bool()

  /** ブランチ命令 */
  val isBranch = Bool()

  /** 分岐ID（BranchBufferで管理） */
  val branchId = UInt(params.branchBufferSize.W)
}

object ReorderBufferEntry {
  def default(implicit params: Parameters): ReorderBufferEntry =
    (new ReorderBufferEntry).Lit(
      _.valueReady -> false.B,
      _.destinationRegister -> 0.reg,
      _.isError -> false.B,
      _.value -> 0.U,
      _.programCounter -> 0.U,
      _.operationInorder -> false.B,
      _.isBranch -> false.B,
      _.canceled -> false.B,
      _.speculative -> false.B,
    )
}
