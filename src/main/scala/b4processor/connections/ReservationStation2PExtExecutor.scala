package b4processor.connections

import b4processor.Parameters
import b4processor.modules.PExt.PExtensionOperation
import b4processor.utils.Tag
import chisel3._

class ReservationStation2PExtExecutor(implicit params: Parameters)
    extends Bundle {
  val destinationTag = new Tag
  val value1 = UInt(64.W)
  val value2 = UInt(64.W)
  val value3 = UInt(64.W)
  val operation = PExtensionOperation()
}
