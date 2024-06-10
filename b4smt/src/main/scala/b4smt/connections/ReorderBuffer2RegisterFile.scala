package b4smt.connections

import b4smt.utils.RVRegister
import chisel3._

class ReorderBuffer2RegisterFile extends Bundle {
  val destinationRegister = new RVRegister()
  val value = UInt(64.W)
}
