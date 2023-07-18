package b4processor.modules.csr

import b4processor.Parameters
import b4processor.utils.Tag
import b4processor.utils.operations.CSROperation
import chisel3._

class CSRReservationStationEntry(implicit params: Parameters) extends Bundle {
  val valid = Bool()
  val sourceTag = new Tag
  val value = UInt(64.W)
  val ready = Bool()
  val destinationTag = new Tag
  val address = UInt(12.W)
  val operation = CSROperation()
}

object CSRReservationStationEntry {
  def default(implicit params: Parameters): CSRReservationStationEntry = {
    val w = Wire(new CSRReservationStationEntry)
    w := DontCare
    w.valid := false.B
    w
  }
}
