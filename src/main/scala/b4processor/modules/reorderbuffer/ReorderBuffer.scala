package b4processor.modules.reorderbuffer

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{
  BranchPrediction2ReorderBuffer,
  CollectedOutput,
  Decoder2ReorderBuffer,
  InstructionStatus,
  ReorderBuffer2CSR,
  ReorderBuffer2RegisterFile,
  ReorderBufferStatusBroadcast,
}
import b4processor.riscv.{CSRs, Causes}
import b4processor.utils.RVRegister.{AddRegConstructor, AddUIntRegConstructor}
import b4processor.utils.{RVRegister, Tag}
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.prefix
import chisel3.util._

object Debug {
  def dbg[T <: Data](t: T): T = {
    printf(p"$t\n")
    t
  }

  def dbghex[T <: Bits](t: T): T = {
    printf("%x\n", t)
    t
  }
}

/** リオーダバッファ
  *
  * @param params
  *   パラメータ
  */
class ReorderBuffer(implicit params: Parameters) extends Module {
  private val tagWidth = params.tagWidth

  val io = IO(new Bundle {
    val decoders =
      Vec(params.decoderPerThread, Flipped(new Decoder2ReorderBuffer))
    val collectedOutputs = Flipped(new CollectedOutput)
    val registerFile =
      Vec(
        params.maxRegisterFileCommitCount,
        Valid(new ReorderBuffer2RegisterFile()),
      )
    val statusBroadcast = new ReorderBufferStatusBroadcast
    val isEmpty = Output(Bool())
    val csr = new ReorderBuffer2CSR

    val threadId = Input(UInt(log2Up(params.threads).W))

    val head = if (params.debug) Some(Output(UInt(tagWidth.W))) else None
    val tail = if (params.debug) Some(Output(UInt(tagWidth.W))) else None
    val bufferIndex0 =
      if (params.debug) Some(Output(new ReorderBufferEntry)) else None
  })

  val head = RegInit(0.U(tagWidth.W))
  val tail = RegInit(0.U(tagWidth.W))
  val flushing = RegInit(false.B)

  val buffer = RegInit(
    VecInit(Seq.fill(math.pow(2, tagWidth).toInt)(ReorderBufferEntry.default)),
  )

  class RegisterTagMapContent extends Bundle {
    val valid = Bool()
    val tagId = UInt(tagWidth.W)
  }

  private object RegisterTagMapContent {
    def default: RegisterTagMapContent =
      new RegisterTagMapContent().Lit(_.valid -> false.B)
    def apply(tagId: UInt): RegisterTagMapContent = {
      val w = Wire(new RegisterTagMapContent)
      w.valid := true.B
      w.tagId := tagId
      w
    }
  }

  io.csr.mcause.valid := false.B
  io.csr.mcause.bits := 0.U
  io.csr.mepc.valid := false.B
  io.csr.mepc.bits := 0.U

  private val registerTagMap = RegInit(
    VecInit(Seq.fill(32)(RegisterTagMapContent.default)),
  )

  class DecoderMap extends Bundle {
    val valid = Bool()
    val tagId = UInt(tagWidth.W)
    val destinationRegister = new RVRegister()
  }

  private object DecoderMap {
    def default: DecoderMap =
      new DecoderMap().Lit(
        _.valid -> false.B,
        _.tagId -> 0.U,
        _.destinationRegister -> 0.reg,
      )
  }

  private val previousDecoderMap =
    Seq.fill(params.decoderPerThread - 1)(WireDefault(DecoderMap.default))
  // レジスタファイルへの書き込み
  private var lastValid = true.B
  for (lsq <- io.statusBroadcast.instructions) {
    lsq.valid := false.B
    lsq.bits := 0.U.asTypeOf(new ReorderBufferStatusBroadcast)
  }
  io.statusBroadcast.isError := false.B
  for (
    ((rf, status), i) <- io.registerFile
      .zip(io.statusBroadcast.instructions)
      .zipWithIndex
  ) {
    prefix(s"to_rf$i") {
      val index = tail + i.U
      val biVal = buffer(index)

      if (i == 0) when(biVal.operationInorder) {
        status.valid := true.B
        status.bits.destinationTag.id := index
        status.bits.destinationTag.threadId := io.threadId
        status.bits.status := InstructionStatus.Confirmed
        biVal.operationInorder := false.B
      }

      val isError = biVal.isError
      val canceled = biVal.canceled
      val isBranch = biVal.isBranch

      val instructionOk =
        (biVal.valueReady || isError) && !biVal.operationInorder
      val canCommit = lastValid && index =/= head && instructionOk

      rf.valid := canCommit
      rf.bits.value := 0.U
      rf.bits.destinationRegister := 0.reg
      when(canCommit) {
        when(isBranch){
          
        }.otherwise {
          when(!isError) {
            rf.bits.value := biVal.value
            rf.bits.destinationRegister := biVal.destinationRegister
            when(
              index === registerTagMap(biVal.destinationRegister.inner).tagId,
            ) {
              registerTagMap(biVal.destinationRegister.inner) :=
                RegisterTagMapContent.default
            }
            status.valid := true.B
            status.bits.destinationTag.id := index
            status.bits.destinationTag.threadId := io.threadId
            status.bits.status := InstructionStatus.Done
          }.elsewhen(canceled) {
            status.valid := true.B
            status.bits.destinationTag.id := index
            status.bits.destinationTag.threadId := io.threadId
            status.bits.status := InstructionStatus.Canceled
          }.elsewhen(isError) {
            io.csr.mcause.valid := true.B
            io.csr.mcause.bits := biVal.value
            io.csr.mepc.valid := true.B
            io.csr.mepc.bits := biVal.programCounter
            status.valid := true.B
            status.bits.destinationTag.id := index
            status.bits.destinationTag.threadId := io.threadId
            status.bits.status := InstructionStatus.Canceled
            io.statusBroadcast.isError := true.B
            for (b <- buffer) {
              b.canceled := true.B
            }
          }.otherwise {
            assert(false.B, "unreachable")
          }
          biVal := ReorderBufferEntry.default
        }
      }
      lastValid = canCommit
    }
  }
  private val tailDelta = MuxCase(
    params.maxRegisterFileCommitCount.U,
    io.registerFile.zipWithIndex.map { case (entry, index) =>
      !entry.valid -> index.U
    },
  )

  // デコーダからの読み取りと書き込み
  private var insertIndex = head
  private var lastReady = true.B
  for (i <- 0 until params.decoderPerThread) {
    prefix(s"from_dec$i") {
      val decoder = io.decoders(i)
      decoder.ready := lastReady && (insertIndex + 1.U) =/= tail
      when(decoder.valid && decoder.ready) {
        buffer(insertIndex) := {
          val entry = Wire(new ReorderBufferEntry)
          entry.destinationRegister := decoder.destination.destinationRegister
          entry.operationInorder := decoder.destination.operationInorder
          entry.programCounter := decoder.programCounter
          entry.isError := decoder.isDecodeError
          entry.value := Mux(
            decoder.isDecodeError,
            Causes.illegal_instruction.U,
            0.U,
          )
          entry.valueReady := false.B
          entry
        }
      }
      if (i < params.decoderPerThread - 1) {
        previousDecoderMap(i).valid :=
          decoder.valid && decoder.destination.destinationRegister =/= 0.reg
        previousDecoderMap(i).tagId := insertIndex
        previousDecoderMap(i).destinationRegister :=
          decoder.destination.destinationRegister
      }
      decoder.destination.destinationTag := Tag(io.threadId, insertIndex)
      registerTagMap(decoder.destination.destinationRegister.inner) :=
        RegisterTagMapContent(insertIndex)

      locally {
        for (source_index <- 0 until 3) {
          prefix(s"prev_dec_check_${i}_src$source_index") {
            val matches_prev_decoder = (0 until i)
              .map(n =>
                (previousDecoderMap(n).valid &&
                  previousDecoderMap(n).destinationRegister
                  === decoder.sources(source_index).sourceRegister),
              )
              .fold(false.B)(_ || _)
            val matchingBits = MuxCase(
              registerTagMap(
                decoder.sources(source_index).sourceRegister.inner,
              ),
              (0 until i)
                .map(n =>
                  (previousDecoderMap(n).valid &&
                    previousDecoderMap(n).destinationRegister
                    === decoder.sources(source_index).sourceRegister) ->
                    RegisterTagMapContent(previousDecoderMap(n).tagId),
                )
                .reverse,
            )
            val hasMatching = matchingBits.valid
            val matchingBuf = buffer(matchingBits.tagId)

            decoder.sources(source_index).matchingTag.valid := hasMatching
            decoder.sources(source_index).matchingTag.bits := Tag(
              io.threadId,
              matchingBits.tagId,
            )
            decoder
              .sources(source_index)
              .value
              .valid := matchingBuf.valueReady && !matches_prev_decoder
            decoder.sources(source_index).value.bits := matchingBuf.value
          }
        }
      }

      // 次のループで使用するinserIndexとlastReadyを変える
      // わざと:=ではなく=を利用している
      insertIndex =
        Mux(decoder.valid && decoder.ready, insertIndex + 1.U, insertIndex)
      lastReady = decoder.ready
    }
  }
  head := insertIndex
  tail := tail + tailDelta
  io.csr.retireCount := PopCount(
    io.statusBroadcast.instructions.map(i =>
      i.valid && i.bits.status === InstructionStatus.Done,
    ),
  )
  io.csr.cancelCount := PopCount(
    io.statusBroadcast.instructions.map(i =>
      i.valid && i.bits.status === InstructionStatus.Canceled,
    ),
  )
  io.isEmpty := head === tail

  // 出力の読み込み
  private val output = io.collectedOutputs.outputs
  for (o <- output) {
    when(o.valid && (o.bits.tag.threadId === io.threadId)) {
      val writePort = buffer(o.bits.tag.id)
      writePort.value := o.bits.value
      writePort.isError := o.bits.isError
      writePort.valueReady := true.B
    }
  }

  registerTagMap(0) := new RegisterTagMapContent()
    .Lit(_.valid -> false.B, _.tagId -> 0.U)

  // デバッグ
  if (params.debug) {
    io.head.get := head
    io.tail.get := tail
    io.bufferIndex0.get := buffer(0)
    //    printf(p"reorder buffer pc=${buffer(0).programCounter} value=${buffer(0).value} ready=${buffer(0).ready} rd=${buffer(0).destinationRegister}\n")
  }

  for (d <- io.decoders)
    for (s <- d.sources)
      s.matchingTag.bits.threadId := io.threadId

  // FORMAL
  for (d <- io.decoders)
    for (s <- d.sources)
      assert(s.matchingTag.bits.threadId === io.threadId)
}

object ReorderBuffer extends App {
  implicit val params =
    Parameters(
      threads = 2,
      decoderPerThread = 2,
      maxRegisterFileCommitCount = 2,
    )
  ChiselStage.emitSystemVerilogFile(new ReorderBuffer)
}
