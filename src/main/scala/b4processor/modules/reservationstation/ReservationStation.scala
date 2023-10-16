package b4processor.modules.reservationstation

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{
  CollectedOutput,
  Decoder2ReservationStation,
  ReservationStation2Executor,
  ReservationStation2PExtExecutor,
}
import b4processor.utils.{B4RRArbiter, MMArbiter}
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._

class ReservationStation(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val collectedOutput = Flipped(new CollectedOutput)
    val issue =
      Vec(params.decoderPerThread, Irrevocable(new ReservationStation2Executor))
    val pextIssue = Vec(
      params.decoderPerThread,
      Irrevocable(new ReservationStation2PExtExecutor()),
    )
//    val issuePext =
//      Vec(params.decoderPerThread, Irrevocable(new ReservationStation2Executor))
    val decoder =
      Vec(params.decoderPerThread, Flipped(new Decoder2ReservationStation))
  })

  val rsWidth = log2Up(params.decoderPerThread * 2)

  val reservation = RegInit(
    VecInit(
      Seq.fill(math.pow(2, rsWidth).toInt)(ReservationStationEntry.default),
    ),
  )

  private val outputArbiter = Module(
    new MMArbiter(
      new ReservationStation2Executor,
      reservation.length,
      params.decoderPerThread,
    ),
  )

  private val pextOutputArbiter = Module(
    new MMArbiter(
      new ReservationStation2PExtExecutor(),
      reservation.length,
      params.decoderPerThread,
    ),
  )

  for (i <- 0 until reservation.length) {
    prefix(s"issue${i % params.decoderPerThread}_resv$i") {
      val a = outputArbiter.io.input(i)
      val r = reservation(i)
      a.bits.operation := r.operation
      a.bits.destinationTag := r.destinationTag
      a.bits.value1 := r.sources(0).getValueUnsafe
      a.bits.value2 := r.sources(1).getValueUnsafe
      a.bits.wasCompressed := r.wasCompressed
      a.bits.branchOffset := r.branchOffset
      a.valid := r.valid &&
        r.sources(0).isValue &&
        r.sources(1).isValue &&
        !r.ispext
      when(a.valid && a.ready) {
        r := ReservationStationEntry.default
      }
    }
    io.issue(i % params.decoderPerThread) <> outputArbiter.io.output(
      i % params.decoderPerThread,
    )
  }

  for (i <- 0 until reservation.length) {
    prefix(s"issue${i % params.decoderPerThread}_resv$i") {
      val a = pextOutputArbiter.io.input(i)
      val r = reservation(i)
      a.bits.operation := r.pextOperation
      a.bits.destinationTag := r.destinationTag
      a.bits.value1 := r.sources(0).getValueUnsafe
      a.bits.value2 := r.sources(1).getValueUnsafe
      a.bits.value3 := r.sources(2).getValueUnsafe
      a.valid := r.valid &&
        r.sources(0).isValue &&
        r.sources(1).isValue &&
        r.sources(2).isValue &&
        r.ispext
      when(a.valid && a.ready) {
        r := ReservationStationEntry.default
      }
    }
    io.pextIssue(i % params.decoderPerThread) <> pextOutputArbiter.io.output(
      i % params.decoderPerThread,
    )
  }

  // デコーダから
  private val head = RegInit(0.U(rsWidth.W))
  private var nextHead = head
  for (i <- 0 until params.decoderPerThread) {
    prefix(s"decoder$i") {
      val decoder = io.decoder(i)
      val resNext = reservation(nextHead)
      decoder.ready := false.B
      when(!resNext.valid) {
        decoder.ready := true.B
        when(decoder.entry.valid) {
          resNext := decoder.entry
        }
      }
      nextHead = nextHead + Mux(!resNext.valid && decoder.entry.valid, 1.U, 0.U)
    }
  }
  head := nextHead

  for ((o, i) <- io.collectedOutput.outputs.zipWithIndex) {
    prefix(s"out$i") {
      when(o.valid) {
        for (entry <- reservation) {
          when(entry.valid) {
            for (source <- entry.sources) {
              source := source.matchExhaustive(
                { tag =>
                  Mux(
                    tag === o.bits.tag,
                    source.fromValue(o.bits.value),
                    source.fromTag(tag),
                  )
                },
                { v => source.fromValue(v) },
              )
            }
          }
        }
      }

    }
  }
}

object ReservationStation extends App {
  implicit val params =
    Parameters(tagWidth = 3, decoderPerThread = 2, threads = 2)
  ChiselStage.emitSystemVerilogFile(new ReservationStation())
}
