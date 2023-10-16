package b4processor.modules.reservationstation

import _root_.circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.ReservationStation2PExtExecutor
import b4processor.utils.MMArbiter
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._

import scala.math.pow

class IssueBuffer2[T <: Data](outputs: Int, t: T)(implicit params: Parameters)
    extends Module {
  val io = IO(new Bundle {
    val reservationStations =
      Vec(params.threads, Vec(params.decoderPerThread, Flipped(Decoupled(t))))
    val executors =
      Vec(outputs, Decoupled(t))
  })

  private val widthPerThread = log2Up(params.decoderPerThread * 2)

  val heads = RegInit(VecInit(Seq.fill(params.threads)(0.U(widthPerThread.W))))
  val buffers = Seq.fill(params.threads)(Mem(pow(2, widthPerThread).toInt, t))
  val buffers_valid = RegInit(
    VecInit(
      Seq.fill(params.threads)(
        VecInit(Seq.fill(pow(2, widthPerThread).toInt)(false.B)),
      ),
    ),
  )

  for (t <- 0 until params.threads) {
    prefix(s"thread$t") {
      val head = heads(t)
      val buffer = buffers(t)
      val buffer_valid = buffers_valid(t)

      val input = io.reservationStations(t)
      var nextHead = head;
      for (i <- input) {
        i.ready := false.B
        when(!buffer_valid(nextHead) && i.valid) {
          buffer_valid(nextHead) := true.B
          buffer(nextHead) := i.bits
          i.ready := true.B
        }
        nextHead =
          Mux(!buffer_valid(nextHead) && i.valid, nextHead + 1.U, nextHead)
      }
      head := nextHead
    }
  }

  val arbiter = Module(
    new MMArbiter(t, params.threads * pow(2, widthPerThread).toInt, outputs),
  )

  for (t <- 0 until params.threads) {
    for (b <- 0 until pow(2, widthPerThread).toInt) {
      val buf = buffers(t)(b)
      val valid = buffers_valid(t)(b)
      val arb = arbiter.io.input(t + b * params.threads)
      arb.bits := buf
      arb.valid := valid
      when(valid && arb.ready) {
        valid := false.B
      }
    }
  }

  for (e <- 0 until outputs) {
    io.executors(e) <> arbiter.io.output(e)
  }
}

object IssueBuffer2 extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(
    new IssueBuffer2(params.executors, new ReservationStation2PExtExecutor),
  )
}
