package b4processor.modules.registerfile

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{
  Decoder2RegisterFile,
  ReorderBuffer2RegisterFile,
}
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._

/** レジスタファイル
  *
  * @param params
  *   パラメータ
  */
class RegisterFile(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {

    /** デコーダへ */
    val decoders =
      Flipped(Vec(params.decoderPerThread, new Decoder2RegisterFile))

    /** リオーダバッファ */
    val reorderBuffer = Flipped(
      Vec(
        params.maxRegisterFileCommitCount,
        Valid(new ReorderBuffer2RegisterFile()),
      ),
    )

    val threadId = Input(UInt(log2Up(params.threads).W))

    val values = if (params.debug) Some(Output(Vec(32, UInt(64.W)))) else None
  })

  /** レジスタx1~x31 tpのみthreadIdで初期化
    */
//  val registers = RegInit(VecInit(Seq.fill(32)(-1.S(64.W).asUInt)))
  val registers = Reg(Vec(32, UInt(64.W)))
//  val registers = Mem(32, UInt(64.W))

  for ((rb, i) <- io.reorderBuffer.zipWithIndex) {
    prefix(s"in${i}") {
      val valid = rb.valid && rb.bits.destinationRegister.inner =/= 0.U
      when(valid) {
        registers(rb.bits.destinationRegister.inner) := rb.bits.value
      }
    }
  }

  // それぞれのデコーダへの信号
  for (dec <- io.decoders) {
    // ソースレジスタが0ならば0それ以外ならばレジスタから
    dec.values zip dec.sourceRegisters foreach { case (v, s) =>
      v := registers(s.inner)
    }
  }

  registers(0) := 0.U

  // デバッグ用信号
  if (params.debug) {
    for (i <- 0 until 32)
      io.values.get(i) := registers(i)
  }
}

object RegisterFile extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(
    new RegisterFile,
    Array(),
    Array("--disable-all-randomization"),
  )
}
