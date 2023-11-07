package b4processor.connections

import b4processor.Parameters
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._

class BranchOutput(implicit params: Parameters) extends Bundle {
  val threadId = UInt(log2Ceil(params.threads).W)
  val programCounterOffset = SInt(64.W)
}

object BranchOutput {
  def noResult()(implicit params: Parameters): BranchOutput =
    (new BranchOutput).Lit(_.threadId -> 0.U, _.programCounterOffset -> 0.S)

  def branch(threadId: UInt, offset: SInt)(implicit
    params: Parameters,
  ): BranchOutput =
    (new BranchOutput)
      .Lit(_.threadId -> threadId, _.programCounterOffset -> offset)
}
