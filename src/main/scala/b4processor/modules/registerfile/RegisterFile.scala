package b4processor.modules.registerfile

import b4processor.Parameters
import b4processor.connections.{
  Decoder2RegisterFile,
  ReorderBuffer2RegisterFile
}
import chisel3._
import chisel3.stage.ChiselStage
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
        Valid(new ReorderBuffer2RegisterFile())
      )
    )

    val threadId = Input(UInt(log2Up(params.threads).W))

    val values = if (params.debug) Some(Output(Vec(32, UInt(64.W)))) else None
  })

  /** レジスタx1~x31 tpのみthreadIdで初期化
    */
  val registers = RegInit(VecInit(Seq.fill(32)(0.U(64.W))))

  for (rb <- io.reorderBuffer) {
    when(rb.valid && rb.bits.destinationRegister =/= 0.U) {
      registers(rb.bits.destinationRegister) := rb.bits.value
    }
  }

  // それぞれのデコーダへの信号
  for (dec <- io.decoders) {
    // ソースレジスタが0ならば0それ以外ならばレジスタから
    dec.value1 := registers(dec.sourceRegister1)
    dec.value2 := registers(dec.sourceRegister2)
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
  (new ChiselStage).emitVerilog(
    new RegisterFile,
    args = Array(
      "--emission-options=disableMemRandomization,disableRegisterRandomization"
    )
  )
}
