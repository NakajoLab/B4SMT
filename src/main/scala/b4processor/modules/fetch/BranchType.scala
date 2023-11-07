package b4processor.modules.fetch

import chisel3._

/** フェッチ用分岐の種類 */
object BranchType extends ChiselEnum {

  /** 分岐なし4byte進む */
  val Next4 = Value

  /** 分岐なし2byte進む */
  val Next2 = Value

  /** 分岐命令 */
  val Branch = Value

  /** JAL命令による強制分岐 */
  val JAL = Value

  /** JALRによる分岐 */
  val JALR = Value

  /** Fenceによる停止 */
  val Fence = Value

  /** Fense.iによる停止 */
  val FenceI = Value

  /** mret */
  val mret = Value

  /** ebreak */
  val Ebreak = Value

  val Wfi = Value
}
