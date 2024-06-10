package b4smt.connections

import b4smt.Parameters
import chisel3._

class InstructionMemory2Cache(implicit params: Parameters) extends Bundle {
  val address = Input(SInt(64.W))
  val output = Output(Vec(params.fetchWidth, UInt(32.W)))
}
