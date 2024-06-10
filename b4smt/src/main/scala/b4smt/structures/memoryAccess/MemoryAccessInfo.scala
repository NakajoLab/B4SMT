package b4smt.structures.memoryAccess

import chisel3._

class MemoryAccessInfo extends Bundle {
  val accessType = MemoryAccessType.Type()
  val signed = Bool()
  val accessWidth = MemoryAccessWidth.Type()
}

object MemoryAccessInfo {
  def apply(opcode: UInt, funct3: UInt): MemoryAccessInfo = {
    val w = Wire(new MemoryAccessInfo)
    w.accessType := MemoryAccessType.fromOpcode(opcode)
    w.signed := !funct3(2)
    w.accessWidth := MemoryAccessWidth.fromFunct3(funct3)
    w
  }
}
