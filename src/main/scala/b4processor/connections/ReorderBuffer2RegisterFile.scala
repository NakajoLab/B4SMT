package b4processor.connections

import b4processor.utils.RVRegister
import chisel3._

class ReorderBuffer2RegisterFile extends Bundle {
  val destinationRegister = new RVRegister()
  val value = UInt(64.W)
}
