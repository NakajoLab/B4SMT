package b4smt.connections

import b4smt.Parameters
import b4smt.utils.Tag
import b4smt.utils.operations.ALUOperation
import chisel3._

class ReservationStation2Executor(implicit params: Parameters) extends Bundle {
  val destinationTag = new Tag
  val value1 = UInt(64.W)
  val value2 = UInt(64.W)
  val operation = ALUOperation()
  val branchOffset = SInt(12.W)
  val wasCompressed = Bool()
}
