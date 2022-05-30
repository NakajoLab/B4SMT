package b4processor.connections

import b4processor.Parameters
import chisel3._

class Fetch2BranchPrediction(implicit params: Parameters) extends Bundle {
  val addressLowerBits = Output(UInt(params.branchPredictionWidth.W))
  val isBranch = Output(Bool())
  val prediction = Input(Bool())
}
