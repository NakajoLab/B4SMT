package b4processor.structures.memoryAccess

import chisel3._
import chisel3.util._

object MemoryAccessWidth extends ChiselEnum {
  val Byte, HalfWord, Word, DoubleWord = Value

  def fromFunct3(funct3: UInt): MemoryAccessWidth.Type = {
    Mux1H(
      Seq(
        (funct3(1, 0) === "b00".U) -> MemoryAccessWidth.Byte,
        (funct3(1, 0) === "b01".U) -> MemoryAccessWidth.HalfWord,
        (funct3(1, 0) === "b10".U) -> MemoryAccessWidth.Word,
        (funct3(1, 0) === "b11".U) -> MemoryAccessWidth.DoubleWord
      )
    )
  }
}
