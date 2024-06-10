package b4smt.connections

import chisel3._

class BranchPrediction2ReorderBuffer extends Bundle {
  val valid = Output(Bool())
  val wasCorrect = Output(Bool())
}
