package b4smt.connections

import chisel3._
class CSR2Fetch extends Bundle {
  val mtvec = UInt(64.W)
  val mepc = UInt(64.W)
  val mcause = UInt(64.W)
}
