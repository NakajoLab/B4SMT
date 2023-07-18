package b4processor.modules.registerfile

import b4processor.Parameters
import b4processor.connections.{
  Decoder2RegisterFile,
  ReorderBuffer2RegisterFile
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
class RegisterFileMem(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {

    /** デコーダへ */
    val decoders =
      Flipped(Vec(params.decoderPerThread, new Decoder2RegisterFile))

    /** リオーダバッファ */
    val reorderBuffer = Flipped(
      Vec(
        params.maxRegisterFileCommitCount,
        Valid(new ReorderBuffer2RegisterFile())
      )
    )

    val threadId = Input(UInt(log2Up(params.threads).W))

    val values = if (params.debug) Some(Output(Vec(32, UInt(64.W)))) else None
  })

  val registers = Mem(31, UInt(64.W))

  for ((rb, i) <- io.reorderBuffer.zipWithIndex) {
    prefix(s"in$i") {
      val valid = rb.valid && rb.bits.destinationRegister =/= 0.reg
      // この先のレジスらにかぶりがある
      val overlapping =
        if (io.reorderBuffer.drop(i + 1).nonEmpty)
          io.reorderBuffer
            .drop(i + 1)
            .map(_.bits.destinationRegister === rb.bits.destinationRegister)
            .reduce(_ || _)
        else false.B
      when(valid && !overlapping) {
        registers.write(rb.bits.destinationRegister.inner - 1.U, rb.bits.value)
      }
    }
  }

  // それぞれのデコーダへの信号
  for ((dec, i) <- io.decoders.zipWithIndex) {
    prefix(s"for_decoder_$i") {
      // ソースレジスタが0ならば0それ以外ならばレジスタから
      dec.value1 := 0.U
      dec.value2 := 0.U
      when(dec.sourceRegister1 =/= 0.reg) {
        dec.value1 := registers.read(dec.sourceRegister1.inner - 1.U)
      }
      when(dec.sourceRegister2 =/= 0.reg) {
        dec.value2 := registers.read(dec.sourceRegister2.inner - 1.U)
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

object RegisterFileMem extends App {
  implicit val params = Parameters(maxRegisterFileCommitCount = 2)
  ChiselStage.emitSystemVerilogFile(
    new RegisterFileMem,
    Array(),
    Array("--disable-all-randomization")
  )
}
