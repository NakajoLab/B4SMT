package b4smt.connections

import b4smt.Parameters
import chisel3._
import chisel3.util._

class InstructionMemoryInterface2Cache(implicit params: Parameters)
    extends Bundle {
  val address = Input(SInt(64.W))
  val output = Valid(UInt((32 * params.fetchWidth).W))
}
