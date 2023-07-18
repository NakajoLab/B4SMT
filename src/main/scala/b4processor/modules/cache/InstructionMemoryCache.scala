package b4processor.modules.cache

import b4processor.Parameters
import b4processor.connections.InstructionCache2Fetch
import b4processor.modules.memory.{InstructionResponse, MemoryReadTransaction}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

/** 命令キャッシュモジュール
  *
  * とても単純なキャッシュ機構
  */
class InstructionMemoryCache(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {

    /** フェッチ */
    val fetch = Vec(params.decoderPerThread, new InstructionCache2Fetch)

    val memory = new Bundle {
      val request = Irrevocable(new MemoryReadTransaction())
      val response = Flipped(Valid(new InstructionResponse))
    }

    val threadId = Input(UInt(log2Up(params.threads).W))
  })

  private class CacheBufferEntry extends Bundle {
    val valid = Bool()
    val upper = UInt(60.W)
    val data = Vec(8, UInt(16.W))
  }
  private object CacheBufferEntry {
    def default: CacheBufferEntry = {
      val w = Wire(new CacheBufferEntry)
      w.valid := false.B
      w.upper := DontCare
      w.data.foreach(p => p := DontCare)
      w
    }
  }
  private val buf = RegInit(VecInit(Seq.fill(4)(CacheBufferEntry.default)))

  private val request = WireDefault(0.U(60.W))
  private var didRequest = false.B
  for (f <- io.fetch) {
    val lowerAddress = f.address.bits(63, 1)
    val upperAddress = lowerAddress + 1.U
    val lowerData = WireDefault(0.U(16.W))
    val upperData = WireDefault(0.U(16.W))
    f.output.valid := false.B
    f.output.bits := 0.U
    var foundData = false.B
    for (b <- buf) {
      when(!foundData && b.valid && lowerAddress(62, 3) === b.upper) {
        lowerData := b.data(lowerAddress(2, 0))
      }
      foundData = foundData || (b.valid && lowerAddress(62, 3) === b.upper)
    }

    var foundData2 = false.B
    for (b <- buf) {
      when(!foundData2 && b.valid && upperAddress(62, 3) === b.upper) {
        upperData := b.data(upperAddress(2, 0))
      }
      foundData2 = foundData2 || (b.valid && upperAddress(62, 3) === b.upper)
    }

    f.output.valid := foundData && foundData2
    f.output.bits := upperData ## lowerData

    when(f.address.valid) {
      when(!foundData && !didRequest) {
        request := lowerAddress(62, 3)
      }.elsewhen(!foundData2 && !didRequest) {
        request := upperAddress(62, 3)
      }
    }
    didRequest = didRequest || (!foundData || !foundData2) && f.address.valid
  }

  private val waiting :: requesting :: Nil = Enum(2)
  private val state = RegInit(waiting)
  private val readIndex = Reg(UInt(1.W))
  private val requested = RegInit(0.U(60.W))
  private val transaction = Reg(new MemoryReadTransaction)
  private val requestDone = Reg(Bool())

  when(didRequest && state === waiting) {
    state := requesting
    requested := false.B
    readIndex := 0.U
    requested := request

    val tmp_transaction =
      MemoryReadTransaction.ReadInstruction(
        Cat(request, 0.U(4.W)),
        2,
        io.threadId
      )
    transaction := tmp_transaction
    io.memory.request.valid := true.B
    io.memory.request.bits := tmp_transaction
    requestDone := false.B
  }

  io.memory.request.valid := false.B
  io.memory.request.bits := DontCare
  private val head = RegInit(0.U(2.W))
  when(state === requesting) {
    when(!requestDone) {
      io.memory.request.valid := true.B
      io.memory.request.bits := transaction
      when(io.memory.request.ready) {
        requestDone := true.B
      }
    }

    when(io.memory.response.valid) {
      buf(head).valid := false.B
      for (i <- 0 until 4) {
        buf(head).data(readIndex ## i.U(2.W)) := io.memory.response.bits
          .inner(i * 16 + 15, i * 16)
      }
      readIndex := readIndex + 1.U
      when(readIndex === 1.U) {
        state := waiting
        buf(head).valid := true.B
        buf(head).upper := requested
        head := head + 1.U
      }
    }
  }

}

object InstructionMemoryCache extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(new InstructionMemoryCache)
}
