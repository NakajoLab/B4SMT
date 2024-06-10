package b4smt.connections

import b4smt.Parameters
import b4smt.utils.Tag
import b4smt.utils.operations.CSROperation
import chisel3._

trait ZeroValue {
  def zeroValue: ZeroValue
}

class Decoder2CSRReservationStation(implicit params: Parameters)
    extends Bundle
    with ZeroValue {
  val sourceTag = new Tag
  val destinationTag = new Tag
  val value = UInt(64.W)
  val ready = Bool()
  val address = UInt(12.W)
  val operation = CSROperation()

  override def zeroValue = 0.U.asTypeOf(new Decoder2CSRReservationStation)
}
