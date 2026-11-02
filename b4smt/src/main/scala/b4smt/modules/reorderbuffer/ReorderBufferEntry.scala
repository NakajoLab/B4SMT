package b4smt.modules.reorderbuffer

import b4smt.utils.RVRegister
import b4smt.utils.RVRegister.AddRegConstructor
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor

class ReorderBufferEntry extends Bundle {

  /** Destination register */
  val destinationRegister = new RVRegister()

  /** Instruction processing is complete (ready to commit) */
  val valueReady = Bool()

  /** Normally: execution result value; on exception: error type */
  val value = UInt(64.W)

  /** Program counter */
  val programCounter = UInt(64.W)

  val operationInorder = Bool()

  /** isError */
  val isError = Bool()
}

object ReorderBufferEntry {
  def default: ReorderBufferEntry =
    (new ReorderBufferEntry).Lit(
      _.valueReady -> false.B,
      _.destinationRegister -> 0.reg,
      _.isError -> false.B,
      _.value -> 0.U,
      _.programCounter -> 0.U,
      _.operationInorder -> false.B,
    )
}
