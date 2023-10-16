package b4processor.modules.cache

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{InstructionCache2Fetch, InstructionMemory2Cache}
import b4processor.modules.memory.{
  InstructionResponse,
  MemoryReadChannel,
  MemoryReadRequest,
}
import b4processor.utils.Tag
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._

import scala.math.pow

class InstructionCache2WaySetAssosiative(cacheTagWidth: Int, offsetWidth: Int)(
  implicit params: Parameters,
) extends Module {
  val io = IO(new Bundle {

    /** フェッチ */
    val fetch = Vec(params.decoderPerThread, new InstructionCache2Fetch)

    val memory = new MemoryReadChannel()

    val threadId = Input(UInt(log2Up(params.threads).W))
  })

  private val set1 = SyncReadMem(32, UInt(256.W))
  private val set2 = SyncReadMem(32, UInt(256.W))
  private val set1Valid = RegInit(VecInit(Seq.fill(32)(false.B)))
  private val set2Valid = RegInit(VecInit(Seq.fill(32)(false.B)))
  private val lastAccess = RegInit(VecInit(Seq.fill(32)(false.B)))

}

object InstructionCache2WaySetAssosiative extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(new InstructionMemoryCache())
}

class Way(tagWidth: Int, offsetWidth: Int) extends Module {
  val read = IO(new Bundle {
    val addressTag = Input(UInt(tagWidth.W))
    val set = Output(UInt(tagWidth.W))
    val cacheHit = Output(Bool())
  })

  val update = IO(Flipped(Valid(new Bundle {
    val addressUpper = UInt((64 - tagWidth - offsetWidth).W)
    val tagListIndex = UInt(tagWidth.W)
  })))

  private val tagList = RegInit(
    VecInit(Seq.fill(pow(2, tagWidth).toInt)(new Bundle {
      val valid = Bool()
      val entry = UInt((64 - tagWidth - offsetWidth).W)
    })),
  )

  read.cacheHit :=
    tagList
      .map(p => p.valid && p.entry === read.addressTag)
      .reduce(_ | _)
  read.set := Mux1H(
    (0 until pow(2, tagWidth).toInt)
      .zip(tagList)
      .map(i => (i._2.valid && i._2.entry === read.addressTag) -> i._1.U),
  )

  when(update.valid) {
    tagList(update.bits.tagListIndex) := update.bits.addressUpper
  }
}

class Set(tagWidth: Int, offsetWidth: Int, blockSize: Int = 64) extends Module {
  val read = IO(new Bundle {
    val addressOffset = Input(UInt(offsetWidth.W))
    val set = Input(UInt(tagWidth.W))
    val output = Output(UInt(blockSize.W))
  })

  val update = IO(Flipped(Valid(new Bundle {
    val offset = Input(UInt(offsetWidth.W))
    val set = Input(UInt(tagWidth.W))
    val data = Input(UInt(blockSize.W))
  })))

  private val set = RegInit(
    VecInit(
      Seq.fill(pow(2, tagWidth).toInt)(
        VecInit(Seq.fill(pow(2, offsetWidth).toInt)(UInt(blockSize.W))),
      ),
    ),
  )

  read.output := set(read.set)(read.addressOffset)

  when(update.valid) {
    set(update.bits.set)(update.bits.offset) := update.bits.data
  }
}
