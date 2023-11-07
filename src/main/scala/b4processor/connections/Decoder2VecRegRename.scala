package b4processor.connections

import b4processor.Parameters
import b4processor.modules.reservationstation.ReservationStationEntry
import chisel3._

class Decoder2VecRegRename(implicit params: Parameters) extends Bundle {
  val ready = Input(Bool())
  val entry = Output(???)
}
