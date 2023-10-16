package b4processor.modules.reservationstation

import _root_.circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.ReservationStation2PExtExecutor
import b4processor.utils.{MMArbiter, PassthroughBuffer}
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._

import scala.math.pow

class IssueBuffer3[T <: Data](outputs: Int, t: T)(implicit params: Parameters)
    extends Module {
  val io = IO(new Bundle {
    val reservationStations =
      Vec(params.threads, Vec(params.decoderPerThread, Flipped(Decoupled(t))))
    val executors =
      Vec(outputs, Decoupled(t))
  })

  val arbiter = Module(
    new MMArbiter(t, params.threads * params.decoderPerThread, outputs),
  )

  for (t <- 0 until params.threads) {
    for (b <- 0 until params.decoderPerThread) {
      val r = io.reservationStations(t)(b)

      val arb = arbiter.io.input(t + b * params.threads)
      arb.bits := r.bits
      arb.valid := r.valid
      r.ready := arb.ready
    }
  }

  for (e <- 0 until outputs) {
    io.executors(e) <> arbiter.io.output(e)
  }
}

object IssueBuffer3 extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(
    new IssueBuffer3(params.executors, new ReservationStation2PExtExecutor),
  )
}
