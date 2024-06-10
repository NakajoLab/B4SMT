package b4smt.modules.reservationstation

import b4smt.Parameters
import b4smt.connections.{
  CollectedOutput,
  Decoder2ReservationStation,
  ReservationStation2Executor,
  ReservationStation2MulDivExecutor,
  ReservationStation2PExtExecutor,
}
import b4smt.utils.{MMArbiter, TagValueBundle}
import chisel3._
import chisel3.experimental.prefix
import circt.stage.ChiselStage
import chisel3.util._
import chiselformal.FormalTools

class ReservationStation2(implicit params: Parameters)
    extends Module
    with FormalTools {
  val io = IO(new Bundle {
    val collectedOutput = Flipped(new CollectedOutput)
    val issue = Irrevocable(new ReservationStation2Executor)
    val mulDivIssue = Irrevocable(new ReservationStation2MulDivExecutor)
    val pextIssue = Irrevocable(new ReservationStation2PExtExecutor())
    val decoder = Flipped(new Decoder2ReservationStation)
    val threadId = Input(UInt(log2Up(params.threads).W))
    val full = Output(Bool())
  })

  val rsWidth = params.reservationStationWidth

  val reservation = RegInit(
    VecInit(
      Seq.fill(math.pow(2, rsWidth).toInt)(ReservationStationEntry.default),
    ),
  )

  private val outputArbiter = Module(
    new Arbiter(new ReservationStation2Executor, reservation.length),
  )

  private val pextOutputArbiter = Module(
    new Arbiter(new ReservationStation2PExtExecutor(), reservation.length),
  )

  private val mulDivOutputArbiter = Module(
    new Arbiter(new ReservationStation2MulDivExecutor(), reservation.length),
  )

  for (i <- 0 until reservation.length) {
    prefix(s"issue${i % params.decoderPerThread}_resv$i") {
      val a = outputArbiter.io.in(i)
      val r = reservation(i)
      a.bits.operation := r.operation
      a.bits.destinationTag := r.destinationTag
      a.bits.value1 := r.sources(0).value
      a.bits.value2 := r.sources(1).value
      a.bits.wasCompressed := r.wasCompressed
      a.bits.branchOffset := r.branchOffset
      a.valid := r.valid &&
        r.sources(0).isValue &&
        r.sources(1).isValue &&
        r.exeType === ExecutorType.Regular
      when(a.valid && a.ready) {
        r := ReservationStationEntry.default
      }
    }

  }
  io.issue <> outputArbiter.io.out

  for (i <- 0 until reservation.length) {
    prefix(s"issue${i % params.decoderPerThread}_resv$i") {
      val a = pextOutputArbiter.io.in(i)
      val r = reservation(i)
      a.bits.operation := r.pextOperation
      a.bits.destinationTag := r.destinationTag
      a.bits.value1 := r.sources(0).value
      a.bits.value2 := r.sources(1).value
      a.bits.value3 := r.sources(2).value
      a.valid := r.valid &&
        r.sources(0).isValue &&
        r.sources(1).isValue &&
        r.sources(2).isValue &&
        r.exeType === ExecutorType.PExt
      when(a.valid && a.ready) {
        r := ReservationStationEntry.default
      }
    }

  }
  io.pextIssue <> pextOutputArbiter.io.out

  for (i <- 0 until reservation.length) {
    prefix(s"issue${i % params.decoderPerThread}_resv$i") {
      val a = mulDivOutputArbiter.io.in(i)
      val r = reservation(i)
      a.bits.operation := r.mulDivOperation
      a.bits.destinationTag := r.destinationTag
      a.bits.value1 := r.sources(0).value
      a.bits.value2 := r.sources(1).value
      a.valid := r.valid &&
        r.sources(0).isValue &&
        r.sources(1).isValue &&
        r.exeType === ExecutorType.MulDiv
      when(a.valid && a.ready) {
        r := ReservationStationEntry.default
      }
    }

  }
  io.mulDivIssue <> mulDivOutputArbiter.io.out

  // デコーダから
  private val head = RegInit(0.U(rsWidth.W))

  val decoder = io.decoder
  val resNext = reservation(head)
  decoder.ready := false.B
  io.full := resNext.valid
  when(!resNext.valid) {
    decoder.ready := true.B
    when(decoder.entry.valid) {
      resNext := decoder.entry
      when(resNext.sources(0).isTag) {
        assert(resNext.sources(0).tag.threadId === io.threadId)
      }
      when(resNext.sources(1).isTag) {
        assert(resNext.sources(1).tag.threadId === io.threadId)
      }
      when(resNext.sources(2).isTag) {
        assert(resNext.sources(2).tag.threadId === io.threadId)
      }
      head := head + 1.U
    }
  }

  val c = RegInit(0.U(64.W))
  c := c + 1.U

  for ((o, i) <- io.collectedOutput.outputs.zipWithIndex) {
    prefix(s"out$i") {
      when(o.valid) {
        for ((entry, ei) <- reservation.zipWithIndex) {
          when(entry.valid) {
            for (source <- entry.sources) {
              when(source.isTag) {
                assert(
                  source.tag.threadId === io.threadId,
                  p"tid was ${source.tag.threadId} should be ${io.threadId} on $ei on ${c}",
                )
                assert(o.bits.tag.threadId === io.threadId)
              }
              when(source.isTag && source.tag === o.bits.tag) {
                source := TagValueBundle.fromValue(o.bits.value)
              }
            }
          }
        }
      }

    }
  }
}

object ReservationStation2 extends App {
  implicit val params: b4smt.Parameters =
    Parameters(
      tagWidth = 3,
      decoderPerThread = 2,
      threads = 4,
      enablePExt = false,
    )
  ChiselStage.emitSystemVerilogFile(
    new ReservationStation2(),
    firtoolOpts = Array(
//      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb",
    ),
  )
}
