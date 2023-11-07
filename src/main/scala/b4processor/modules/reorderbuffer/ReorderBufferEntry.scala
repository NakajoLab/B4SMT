package b4processor.modules.reorderbuffer

import b4processor._
import b4processor.modules.vector._
import b4processor.utils.RVRegister
import b4processor.utils.RVRegister.AddRegConstructor
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._

class ReorderBufferEntry(implicit params: Parameters) extends Bundle {

  /** デスティネーションレジスタ */
  val destinationRegister = new RVRegister()

  /** 命令の処理が完了した（コミットできる） */
  val valueReady = Bool()

  /** 通常時：実行結果の値 例外時：エラーの種類 */
  val value = UInt(64.W)

  /** プログラムカウンタ */
  val programCounter = UInt(64.W)

  val operationInorder = Bool()

  /** isError */
  val isError = Bool()

  val vecCsrBypass = new Bundle {
    val vtype = new VtypeBundle()
    val vl = UInt((log2Up(params.vlenb)+1).W)
  }
}

// TODO: ベクトルCSRの値を追加する
object ReorderBufferEntry {
  def default(implicit params: Parameters): ReorderBufferEntry =
    (new ReorderBufferEntry).Lit(
      _.valueReady -> false.B,
      _.destinationRegister -> 0.reg,
      _.isError -> false.B,
      _.value -> 0.U,
      _.programCounter -> 0.U,
      _.operationInorder -> false.B,
    )
}
