package b4processor.modules.fetch

import b4processor.Parameters
import b4processor.connections.{Fetch2FetchBuffer, FetchBuffer2Uncompresser}
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

import scala.math.pow

class FetchBuffer(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val output = Vec(params.decoderPerThread, new FetchBuffer2Uncompresser)
    val input = Flipped(new Fetch2FetchBuffer)
  })

  /**
    * Fetchからの入力の数と同じ数の同期バッファを作成する
    * 命令デコードはインオーダであるため，先頭がどのバッファかわかっている必要がある
    */
  val sync_buffer = Seq.fill(io.input.toBuffer.length)(SyncReadMem(pow(2, params.decoderPerThread).toInt, new BufferEntry))

  val heads = Seq.fill(io.input.toBuffer.length)(RegInit(0.U((params.decoderPerThread + 1).W)))
  val tails = Seq.fill(io.input.toBuffer.length)(RegInit(0.U((params.decoderPerThread + 1).W)))
  val all_indexOk = heads.indices.map(i => (heads(i) + 1.U =/= tails(i))).reduce(_ && _)

  val headbuffer = RegInit(0.U(log2Up(io.input.toBuffer.length)))

  val bufferEntry_inputs = Wire(Vec(io.input.toBuffer.length, Valid(new BufferEntry)))
  for (i <- io.input.toBuffer.indices) {
    /*
    bufferEntry_inputs(i).valid := io.input.toBuffer(i).valid
    bufferEntry_inputs(i).bits.instruction := io.input.toBuffer(i).bits.instruction
    bufferEntry_inputs(i).bits.programCounter := io.input.toBuffer(i).bits.programCounter
     */
    bufferEntry_inputs(i) <> io.input.toBuffer(i)
    io.input.toBuffer(i).ready
  }

  for(d <- io.input.toBuffer) {

  }

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
