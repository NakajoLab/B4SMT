package b4processor.modules.vector

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{CollectedOutput, Decoder2ReservationStation, ReservationStation2Executor}
import b4processor.modules.reservationstation._
import b4processor.utils._
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._

class VectorReservationStation(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val scalarCollectedOutput = Flipped(new CollectedOutput())
    val vconfOutput = Flipped(new VconfOutput())
    val issue =
      Vec(params.decoderPerThread, Irrevocable(new ReservationStation2Executor))
    val decoder =
      Vec(params.decoderPerThread, Flipped(new Decoder2ReservationStation))
  })

  val rsWidth = log2Up(params.decoderPerThread * 2)

  val reservation = RegInit(
    VecInit(
      Seq.fill(math.pow(2, rsWidth).toInt)(ReservationStationEntry.default)
    )
  )
}
