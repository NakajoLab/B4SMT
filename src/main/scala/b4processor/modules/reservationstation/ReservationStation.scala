package b4processor.modules.reservationstation

import b4processor.Parameters
import b4processor.connections.{
  CollectedOutput,
  Decoder2ReservationStation,
  ReservationStation2Executor,
  ResultType
}
import b4processor.utils.B4RRArbiter
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

class ReservationStation(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val collectedOutput = Flipped(Vec(params.threads, new CollectedOutput))
    val executor =
      Vec(params.executors, Irrevocable(new ReservationStation2Executor))
    val decoder = Vec(
      params.threads * params.decoderPerThread,
      Flipped(new Decoder2ReservationStation)
    )
  })

  val rsWidth = log2Up(params.threads * params.decoderPerThread * 4)

  val reservation = RegInit(
    VecInit(
      Seq.fill(math.pow(2, rsWidth).toInt)(ReservationStationEntry.default)
    )
  )
  private val outputArbiter = Seq.fill(params.executors)(
    Module(
      new B4RRArbiter(
        new ReservationStation2Executor,
        math.pow(2, rsWidth).toInt / params.executors
      )
    )
  )

  for (eid <- 0 until params.executors) {
    for (i <- 0 until reservation.length / params.executors) {
      val a = outputArbiter(eid).io.in(i)
      val r = reservation(i * params.executors + eid)
      a.bits.opcode := r.opcode
      a.bits.destinationTag := r.destinationTag
      a.bits.value1 := r.value1
      a.bits.value2 := r.value2
      a.bits.function3 := r.function3
      a.bits.immediateOrFunction7 := r.immediateOrFunction7
      a.bits.wasCompressed := r.wasCompressed
      a.valid := r.valid && r.ready1 && r.ready2
      when(a.valid && a.ready) {
        r := ReservationStationEntry.default
      }
    }
    io.executor(eid) <> outputArbiter(eid).io.out
  }

  // デコーダから
  private val head = RegInit(0.U(rsWidth.W))
  private var nextHead = head
  for (i <- 0 until (params.decoderPerThread * params.threads)) {
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
  head := nextHead

  for (output <- io.collectedOutput) {
    when(
      output.outputs.valid && output.outputs.bits.resultType === ResultType.Result
    ) {
      for (entry <- reservation) {
        when(entry.valid) {
          when(!entry.ready1 && entry.sourceTag1 === output.outputs.bits.tag) {
            entry.value1 := output.outputs.bits.value
            entry.ready1 := true.B
          }
          when(!entry.ready2 && entry.sourceTag2 === output.outputs.bits.tag) {
            entry.value2 := output.outputs.bits.value
            entry.ready2 := true.B
          }
        }
      }
    }
  }
}

object ReservationStation extends App {
  implicit val params =
    Parameters(tagWidth = 2, decoderPerThread = 1, threads = 1)
  (new ChiselStage).emitVerilog(
    new ReservationStation(),
    args = Array(
      "--emission-options=disableMemRandomization,disableRegisterRandomization"
    )
  )
}
