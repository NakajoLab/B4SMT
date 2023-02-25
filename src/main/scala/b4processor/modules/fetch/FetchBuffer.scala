package b4processor.modules.fetch

import b4processor.Parameters
import b4processor.connections.{Fetch2FetchBuffer, FetchBuffer2Uncompresser}
import b4processor.utils.FIFO
import chisel3.{util, _}
import chisel3.stage.ChiselStage
import chisel3.util._

import scala.annotation.tailrec
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

  val sync_buffer = Seq.fill(fetchInputLen)(Module(new b4processor.utils.FIFO(log2Up(fetchInputLen))(new BufferEntry)))
  // sync_bufferの入力インタフェース
  val sync_buffer_input = Wire(Vec(fetchInputLen, Irrevocable(new BufferEntry)))
  for ((d,i) <- sync_buffer.zipWithIndex) {
    d.input.bits := sync_buffer_input(i).bits
    d.input.valid := sync_buffer_input(i).valid
    sync_buffer_input(i).ready := d.input.ready
    d.flush := false.B
  }
  // sync_buffer_inputの初期化
  for(d <- sync_buffer_input) {
    d.bits.instruction := 0.U
    d.bits.programCounter := 0.U
    d.valid := false.B
  }
  val sync_buffer_output = Wire(Vec(fetchInputLen, Irrevocable(new BufferEntry)))
  for((d,i) <- sync_buffer.zipWithIndex) {
    sync_buffer_output(i).bits := d.output.bits
    sync_buffer_output(i).valid := d.output.valid
    d.output.ready := sync_buffer_output(i).ready
  }
  // sync_buffer_outputの初期化
  for(d <- sync_buffer_output) {
    d.ready := false.B
  }
  val all_bufferValid = sync_buffer.map(i => !(i.full)).reduce(_ && _)
  io.input.empty := sync_buffer.map(_.empty).reduce(_ && _)

  // Fetchから入力される最初のインデックス
  val buffer_in_ptr = RegInit(0.U(log2Up(fetchInputLen).W))

  // Fetchからの入力をsync_buffer_inputに入れる
  // val bufferEntry_inputs = Wire(Vec(fetchInputLen, Valid(new BufferEntry)))

  val input_validList = io.input.toBuffer.map(_.valid)

  for ((d,i) <- io.input.toBuffer.zipWithIndex) {
    val sync_buffer_write_valid = input_validList.slice(0,i+1).reduce(_ && _)
    sync_buffer_input(buffer_in_ptr+i.U).bits := d.bits
    sync_buffer_input(buffer_in_ptr+i.U).valid := sync_buffer_write_valid && all_bufferValid
    d.ready := all_bufferValid
  }

  private def count_continued_true(num: UInt, ls: Seq[Bool]): UInt = {
    ls.length match {
      case 0 => num
      case _ => Mux(ls.head === true.B, count_continued_true(num+1.U, ls.tail), num)
    }
  }

  @tailrec
  private def rotate_by_vec[T <: Data](rotate_val: Int, ls: Vec[T]): Vec[T] = {
    rotate_val match {
      case 0 => ls
      case _ => rotate_by_vec(rotate_val-1, VecInit(ls.tail :+ ls.head))
    }
  }


  buffer_in_ptr := buffer_in_ptr + count_continued_true(0.U(log2Up(fetchInputLen).W), input_validList)

  // TODO: implement output logic

  for((d,i) <- sync_buffer.zipWithIndex) {
    sync_buffer_output(i.U) <> d.output
  }

  val buffer_out_ptr = RegInit(0.U(log2Up(fetchInputLen).W))
  // val rotated_sync_buffer_index: Vec[UInt] = rotate_vec(buffer_out_ptr, VecInit(sync_buffer.indices.map(_.U(log2Up(fetchInputLen).W))))
  // val rotated_sync_buffer_index = VecInit(sync_buffer.indices.map(_.U(log2Up(fetchInputLen).W)))
  val rotated_sync_buffer_index = Wire(Vec(sync_buffer.length, UInt(log2Up(fetchInputLen).W)))
  for((d,i) <- rotated_sync_buffer_index.zipWithIndex) {
    d := i.U
  }
  for(i <- 0 until fetchInputLen) {
    when(buffer_out_ptr === i.U) {
      val indexes = rotate_by_vec(i, VecInit(sync_buffer.indices.map(_.U(log2Up(fetchInputLen).W))))
      for((d,j) <- rotated_sync_buffer_index.zipWithIndex) {
        d := indexes(j)
      }
    }
  }

  val rotated_sync_buffer: Seq[IrrevocableIO[BufferEntry]] = sync_buffer_output.indices.map(i => sync_buffer_output(rotated_sync_buffer_index(i)))
  val sync_buffer_out_select_index = WireInit(0.U(log2Up(fetchInputLen).W))
  val output_validList = rotated_sync_buffer.map(_.valid)
  val all_decoderValid = io.output.map(_.ready).reduce(_ && _)
  val sync_buffer_read_ready = Wire(Bool())
  sync_buffer_read_ready := false.B

  // output initialisation
  io.output := DontCare

  when(all_decoderValid) {
    for((d,i) <- rotated_sync_buffer.zipWithIndex) {
      sync_buffer_read_ready := output_validList.slice(0, i+1).reduce(_ && _)
      d.ready := sync_buffer_read_ready
      // sync_buffer_out_select_index := buffer_out_ptr + PopCount(output_validList.slice(0, i+1))
      io.output(i).bits.instruction := d.bits.instruction
      io.output(i).bits.programCounter := d.bits.programCounter
      io.output(i).valid := d.valid && sync_buffer_read_ready
    }
  }

  buffer_out_ptr := buffer_out_ptr + count_continued_true(0.U(log2Up(fetchInputLen).W), output_validList)
}

sealed class BufferEntry extends Bundle {
  val instruction = UInt(32.W)
  val programCounter = UInt(64.W)
}

object BufferEntry extends App {
  implicit val params = Parameters(tagWidth = 2, decoderPerThread = 2)
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
