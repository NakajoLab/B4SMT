package b4processor.modules.fetch

import chisel3._

object WaitingReason extends ChiselEnum {

  /** 分岐なし */
  val None = Value

  /** 分岐命令 */
  val Branch = Value

  /** JALRによる分岐 */
  val JALR = Value

  /** Fenceによる停止 */
  val Fence = Value

  /** Fense.iによる停止 */
  val FenceI = Value

  /** JAL命令で同じ場所を移動していると、ReservationStationが圧迫されてしまうので遅延させる */
  val BusyLoop = Value

  /** mret */
  val mret = Value

  /** exception */
  val Exception = Value

  /** WFI */
  val WaitForInterrupt = Value
}
