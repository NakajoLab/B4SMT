package b4processor.modules.csr

import b4processor.Parameters
import b4processor.connections.{
  CSRReservationStation2CSR,
  CollectedOutput,
  Decoder2CSRReservationStation,
  OutputValue
}
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

class CSRReservationStation(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val decoderInput =
      Vec(
        params.decoderPerThread,
        Flipped(Decoupled(new Decoder2CSRReservationStation))
      )
    val toCSR =
      Decoupled(new CSRReservationStation2CSR())
    val output = Flipped(new CollectedOutput())
    val empty = Output(Bool())
  })

  io.toCSR.valid := false.B
  io.toCSR.bits := DontCare

  private val head = RegInit(0.U(2.W))
  private val tail = RegInit(0.U(2.W))
  private val buf = RegInit(
    VecInit(Seq.fill(4)(CSRReservationStationEntry.default))
  )
  io.empty := head === tail

  var insertIndex = head
  for (d <- io.decoderInput) {
    d.ready := tail =/= insertIndex + 1.U
    when(d.ready && d.valid) {
      buf(insertIndex) := {
        val w = Wire(new CSRReservationStationEntry)
        w.valid := true.B
        w.sourceTag := d.bits.sourceTag
        w.destinationTag := d.bits.destinationTag
        w.value := d.bits.value
        w.ready := d.bits.ready
        w.address := d.bits.address
        w.csrAccessType := d.bits.csrAccessType
        w
      }
    }
    insertIndex = Mux(d.ready && d.valid, insertIndex + 1.U, insertIndex)
  }
  head := insertIndex

  val bufTail = buf(tail)
  when(tail =/= head && bufTail.ready) {
    io.toCSR.valid := true.B
    io.toCSR.bits.value := bufTail.value
    io.toCSR.bits.address := bufTail.address
    io.toCSR.bits.destinationTag := bufTail.destinationTag
    io.toCSR.bits.csrAccessType := bufTail.csrAccessType
    when(io.toCSR.ready) {
      tail := tail + 1.U
      bufTail.valid := false.B
    }
  }

  when(io.output.outputs.valid) {
    for (b <- buf) {
      when(b.valid && !b.ready && b.sourceTag === io.output.outputs.bits.tag) {
        b.value := io.output.outputs.bits.value
        b.ready := true.B
      }
    }
  }
}

object CSRReservationStation extends App {
  implicit val params = Parameters()
  (new ChiselStage).emitSystemVerilog(new CSRReservationStation())
}
