package b4processor.modules.fetch

import b4processor.Parameters
import b4processor.connections.{Fetch2FetchBuffer, FetchBuffer2Uncompresser}
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

import scala.math.pow

class FetchBuffer(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    // output(i): Output ReadyValidIO(instruction, programCounter)
    val output = Vec(params.decoderPerThread, new FetchBuffer2Uncompresser)
    // input.toBuffer: Input Vec(params.decoderPerThread, Decoupled(instruction, programCounter))
    // input.empty: Output Bool
    val input = Flipped(new Fetch2FetchBuffer)
  })

  val fetchInputLen = io.input.toBuffer.length

  require(isPow2(fetchInputLen), "number of decoders per thread must be power of 2")

  // Fetchからの入力の数と同じ数の同期バッファを作成する

  val sync_buffer = Seq.fill(fetchInputLen)(new b4processor.utils.FIFO(log2Up(fetchInputLen))(new BufferEntry))
  val sync_buffer_input = Vec(fetchInputLen, Flipped(Irrevocable(new BufferEntry)))
  for ((d,i) <- sync_buffer.zipWithIndex) {
    d.input <> sync_buffer_input
  }
  val all_bufferValid = sync_buffer.map(i => !(i.full)).reduce(_ && _)
  io.input.empty := sync_buffer.map(i => i.empty).reduce(_ && _)

  // Fetchから入力される最初のインデックス
  val buffer_in_ptr = RegInit(0.U(log2Up(fetchInputLen).W))
  val buffer_in_next = Wire(0.U(log2Up(fetchInputLen).W))

  // Fetchからの入力をsync_bufferに入れる
  val bufferEntry_inputs = Wire(Vec(fetchInputLen, Valid(new BufferEntry)))
  for ((d,i) <- io.input.toBuffer.zipWithIndex) {
    bufferEntry_inputs(i.U) <> d
    d.ready := all_bufferValid
  }

  val sync_buffer_select_index = Wire(0.U(log2Up(fetchInputLen).W))
  val input_validList: Seq[Bool] = io.input.toBuffer.map(i => i.valid)

  when(all_bufferValid) {
    for ((d, i) <- sync_buffer.zipWithIndex) {
      sync_buffer_select_index := buffer_in_ptr + PopCount(input_validList.slice(0, i+1))
      when(input_validList(i)) {
         sync_buffer_input(sync_buffer_select_index) := bufferEntry_inputs(i)
      }
    }
  }

  // TODO: implement output logic

  val buffer = Reg(
    Vec(pow(2, params.decoderPerThread + 1).toInt, new BufferEntry)
  )

  val head = RegInit(0.U((params.decoderPerThread + 1).W))
  val tail = RegInit(0.U((params.decoderPerThread + 1).W))
  io.input.empty := head === tail

  {
    var nextHead = head
    for (d <- io.input.toBuffer) {
      val indexOk = nextHead + 1.U =/= tail
      d.ready := indexOk
      val valid = d.valid && indexOk
      when(valid) {
        buffer(nextHead) := BufferEntry.validEntry(
          d.bits.instruction,
          d.bits.programCounter
        )
      }
      nextHead = Mux(valid, nextHead + 1.U, nextHead)
    }
    head := nextHead
  }

  {
    var nextTail = tail
    for (d <- io.output) {
      val indexOk = nextTail =/= head
      d.valid := indexOk
      val valid = d.ready && indexOk
      d.bits.instruction := 0.U
      d.bits.programCounter := 0.U
      when(valid) {
        d.bits.instruction := buffer(nextTail).instruction
        d.bits.programCounter := buffer(nextTail).programCounter
      }
      nextTail = Mux(valid, nextTail + 1.U, nextTail)
    }
    tail := nextTail
  }

}

sealed class BufferEntry extends Bundle {
  val instruction = UInt(32.W)
  val programCounter = UInt(64.W)
}

object BufferEntry extends App {
  implicit val params = Parameters(tagWidth = 2, decoderPerThread = 1)
  (new ChiselStage).emitVerilog(
    new FetchBuffer(),
    args = Array(
      "--emission-options=disableMemRandomization,disableRegisterRandomization"
    )
  )

  def default(): BufferEntry = {
    val w = Wire(new BufferEntry)
    w.instruction := 0.U
    w.programCounter := 0.S
    w
  }

  def validEntry(instruction: UInt, programCounter: UInt): BufferEntry = {
    val w = Wire(new BufferEntry)
    w.instruction := instruction
    w.programCounter := programCounter
    w
  }
}
