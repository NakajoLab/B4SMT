package b4smt.modules.csr

import chisel3._

object MisaGen {
  def apply(): UInt = {
    val bits =
      // XX: MXL
      // XX000000000000000000000000000000000000ZYXWVUTSRQPONMLKJIHGFEDCBA
      "b_1000000000000000000000000000000000000000000000001000000100000101".U
    bits
  }
}
