package b4smt.connections

import b4smt.Parameters
import b4smt.utils.{ForPext, RVRegister}
import chisel3._

/** デコーダとレジスタファイルをつなぐ
  */
class Decoder2RegisterFile(implicit params: Parameters) extends Bundle {
  val sourceRegisters = Output(Vec(3, new RVRegister()))
  val values = Input(Vec(3, UInt(64.W)))
}
