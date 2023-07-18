package b4processor.connections

import b4processor.utils.RVRegister
import chisel3._

/** デコーダとレジスタファイルをつなぐ
  */
class Decoder2RegisterFile extends Bundle {
  val sourceRegister1 = Output(new RVRegister())
  val sourceRegister2 = Output(new RVRegister())
  val value1 = Input(UInt(64.W))
  val value2 = Input(UInt(64.W))
}
