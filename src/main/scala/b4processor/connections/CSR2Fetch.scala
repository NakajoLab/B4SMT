package b4processor.connections

import chisel3._
class CSR2Fetch extends Bundle {
  val mtvec = UInt(64.W)
  val mepc = UInt(64.W)
}
