package b4smt.connections

import b4smt.Parameters
import b4smt.modules.reservationstation.ReservationStationEntry
import chisel3._

class Decoder2ReservationStation(implicit params: Parameters) extends Bundle {
  val ready = Input(Bool())
  val entry = Output(new ReservationStationEntry)
}
