package b4smt.connections

import b4smt.Parameters
import b4smt.utils.Tag
import b4smt.utils.operations.CSROperation
import chisel3._

class CSRReservationStation2CSR(implicit params: Parameters) extends Bundle {
  val address = UInt(12.W)
  val value = UInt(64.W)
  val destinationTag = new Tag
  val operation = CSROperation()
}
