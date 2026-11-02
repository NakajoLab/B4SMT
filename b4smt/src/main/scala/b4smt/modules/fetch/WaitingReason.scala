package b4smt.modules.fetch

import chisel3._

object WaitingReason extends ChiselEnum {

  /** No branch */
  val None = Value

  /** Branch instruction */
  val Branch = Value

  /** Branch via JALR */
  val JALR = Value

  /** Stall due to Fence */
  val Fence = Value

  /** Stall due to Fense.i */
  val FenceI = Value

  /** Delay JAL instructions that jump to the same location to avoid putting
    * pressure on the ReservationStation
    */
  val BusyLoop = Value

  /** mret */
  val mret = Value

  /** exception */
  val Exception = Value

  /** WFI */
  val WaitForInterrupt = Value
}
