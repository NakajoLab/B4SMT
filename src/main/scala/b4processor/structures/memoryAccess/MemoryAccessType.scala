package b4processor.structures.memoryAccess

import chisel3._
import chisel3.util._

object MemoryAccessType extends ChiselEnum {
  val Store, Load = Value

  private val LOAD = "b0000011".U
  private val STORE = "b0100011".U

  def fromOpcode(opcode: UInt): MemoryAccessType.Type = {
    Mux1H(
      Seq(
        (opcode(5) === false.B) -> MemoryAccessType.Load,
        (opcode(5) === true.B) -> MemoryAccessType.Store
      )
    )
  }
}
