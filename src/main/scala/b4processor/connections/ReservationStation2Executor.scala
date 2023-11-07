package b4processor.connections

import b4processor.Parameters
import b4processor.modules.PExt.PExtensionOperation
import b4processor.utils.Tag
import b4processor.utils.operations.ALUOperation
import chisel3._

class ReservationStation2Executor(implicit params: Parameters) extends Bundle {
  val destinationTag = new Tag
  val value1 = UInt(64.W)
  val value2 = UInt(64.W)
  val operation = ALUOperation()
  val branchOffset = SInt(12.W)
  val wasCompressed = Bool()
}
