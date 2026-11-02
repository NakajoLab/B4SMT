package b4smt.modules.fetch

import chisel3._

/** Branch types for fetch */
object BranchType extends ChiselEnum {

  /** No branch; advance 4 bytes */
  val Next4 = Value

  /** No branch; advance 2 bytes */
  val Next2 = Value

  /** Branch instruction */
  val Branch = Value

  /** Unconditional branch via a JAL instruction */
  val JAL = Value

  /** Branch via JALR */
  val JALR = Value

  /** Stall due to Fence */
  val Fence = Value

  /** Stall due to Fense.i */
  val FenceI = Value

  /** mret */
  val mret = Value

  /** ebreak */
  val Ebreak = Value

  val Wfi = Value
}
