package b4processor.modules.registerfile

import b4processor.Parameters
import b4processor.connections.{
  Decoder2RegisterFile,
  ReorderBuffer2RegisterFile,
}
import b4processor.utils.RVRegister.AddRegConstructor
import chisel3._
import chisel3.experimental.prefix
import circt.stage.ChiselStage
import chisel3.util._

/** レジスタファイル
  *
  * @param params
  *   パラメータ
  */
class RegisterFileMultipleMem(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {

    /** デコーダへ */
    val decoders =
      Flipped(Vec(params.decoderPerThread, new Decoder2RegisterFile))

    /** リオーダバッファ */
    val reorderBuffer = Flipped(
      Vec(
        params.maxRegisterFileCommitCount,
        Decoupled(new ReorderBuffer2RegisterFile()),
      ),
    )

    val threadId = Input(UInt(log2Up(params.threads).W))

    val values = if (params.debug) Some(Output(Vec(32, UInt(64.W)))) else None
  })

  val divideIn = 2
  require(isPow2(divideIn))

  val registersPerDecoder = Seq.fill(params.decoderPerThread)(
    Seq.fill(divideIn)(Mem(32 / divideIn, UInt(64.W))),
  )

  val writeIn = Seq.fill(divideIn)(WireDefault(false.B))
  val writeValue = Seq.fill(divideIn)(WireDefault(0.U(64.W)))

  val wroteIn = Seq.fill(divideIn)(
    WireInit(VecInit(Seq.fill(params.maxRegisterFileCommitCount)(false.B))),
  )

  for ((rb, i) <- io.reorderBuffer.zipWithIndex) {
    for (rem <- 0 until divideIn) {
      val valid = rb.valid &&
        rb.bits.destinationRegister
          .inner(log2Ceil(divideIn) - 1, 0) === rem.U &&
        (if (rem == 0) { rb.bits.destinationRegister =/= 0.reg }
         else { true.B })
      val overlapping =
        if (i != 0)
          wroteIn(rem).take(i - 1).reduce(_ || _)
        else
          false.B
      when(valid && !overlapping) {
        writeIn(rem) := true.B
        writeValue(rem) := rb.bits.value
        wroteIn(rem)(i) := true.B
      }
    }
  }

  for (r <- registersPerDecoder) {
    for (rem <- 0 until divideIn) {
      when(writeIn(rem)) {
        r(rem).write(
          io.reorderBuffer(rem)
            .bits
            .destinationRegister
            .inner(4, log2Ceil(divideIn)),
          writeValue(rem),
        )
      }
    }
  }

  // それぞれのデコーダへの信号
  for ((dec, i) <- io.decoders.zipWithIndex) {
    prefix(s"for_decoder_$i") {
      // ソースレジスタが0ならば0それ以外ならばレジスタから
      dec.values := 0.U.asTypeOf(dec.values)
      dec.values zip dec.sourceRegisters foreach { case (v, s) =>
        when(s =/= 0.reg) {
          v := registers.read(s.inner - 1.U)
        }
      }
    }
  }

  // デバッグ用信号
  if (params.debug) {
    io.values.get(0) := 0.U
    for (i <- 1 until 32)
      io.values.get(i) := registers.read((i - 1).U)
  }
}

object RegisterFileMultipleMem extends App {
  implicit val params = Parameters(maxRegisterFileCommitCount = 2)
  ChiselStage.emitSystemVerilogFile(
    new RegisterFileMultipleMem,
    Array(),
    Array("--disable-all-randomization"),
  )
}
