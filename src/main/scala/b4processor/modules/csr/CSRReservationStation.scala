package b4processor.modules.csr

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{
  CSRReservationStation2CSR,
  CollectedOutput,
  Decoder2CSRReservationStation,
  InstructionStatus,
  ReorderBufferStatusBroadcast,
}
import chisel3._
import chisel3.util._

class CSRReservationStation(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val decoderInput =
      Vec(
        params.decoderPerThread,
        Flipped(Decoupled(new Decoder2CSRReservationStation)),
      )
    val toCSR =
      Decoupled(new CSRReservationStation2CSR())
    val output = Flipped(new CollectedOutput())
    val reorderBuffer = new ReorderBufferStatusBroadcast
    val empty = Output(Bool())
    val isError = Input(Bool())
  })

  io.toCSR.valid := false.B
  io.toCSR.bits := DontCare

  private val head = RegInit(0.U(2.W))
  private val tail = RegInit(0.U(2.W))
  private val buf = RegInit(
    VecInit(Seq.fill(4)(CSRReservationStationEntry.default)),
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
        w.operation := d.bits.operation
        w.status := InstructionStatus.Running
        w
      }
    }
    insertIndex = Mux(d.ready && d.valid, insertIndex + 1.U, insertIndex)
  }
  head := insertIndex

  val bufTail = buf(tail)
  when(tail =/= head && bufTail.ready && !io.isError) {
    when(bufTail.status === InstructionStatus.Confirmed) {
      io.toCSR.valid := true.B
      io.toCSR.bits.value := bufTail.value
      io.toCSR.bits.address := bufTail.address
      io.toCSR.bits.destinationTag := bufTail.destinationTag
      io.toCSR.bits.operation := bufTail.operation
      when(io.toCSR.ready) {
        tail := tail + 1.U
        bufTail.valid := false.B
      }
    }.elsewhen(bufTail.status === InstructionStatus.Canceled) {
      tail := tail + 1.U
      bufTail.valid := false.B
    }
  }

  for (o <- io.output.outputs) {
    when(o.valid) {
      for (b <- buf) {
        when(b.valid && !b.ready && b.sourceTag === o.bits.tag) {
          b.value := o.bits.value
          b.ready := true.B
        }
      }
    }
  }

  for (r <- io.reorderBuffer.instructions) {
    when(r.valid) {
      for (b <- buf) {
        when(b.valid && r.bits.destinationTag === b.destinationTag) {
          b.status := r.bits.status
        }
      }
    }
  }

  when(io.isError) {
    tail := insertIndex
  }
}

object CSRReservationStation extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(new CSRReservationStation())
}
