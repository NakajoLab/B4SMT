package b4smt.modules.branchprediction

import b4smt.Parameters
import b4smt.connections.Fetch2BranchPrediction
import chisel3._

/** Branch prediction module */
class BranchPrediction(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val fetch = new Fetch2BranchPrediction
  })
  io.fetch.prediction := false.B
}
