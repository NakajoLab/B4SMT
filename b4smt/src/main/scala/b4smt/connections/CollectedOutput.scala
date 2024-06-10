package b4smt.connections

import b4smt.Parameters
import chisel3._
import chisel3.util._

class CollectedOutput(implicit params: Parameters) extends Bundle {
  val outputs = Vec(params.parallelOutput, Valid(new OutputValue()))
}
