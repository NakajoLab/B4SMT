package b4processor.modules.memory

import b4processor.Parameters
import b4processor.structures.memoryAccess.MemoryAccessWidth
import b4processor.utils.Tag
import chisel3._

class MemoryWriteTransaction(implicit params: Parameters) extends Bundle {
  val outputTag = new Tag()
  val address = UInt(64.W)
  val data = UInt(64.W)
  val mask = UInt(8.W)
}

class MemoryReadTransaction(implicit params: Parameters) extends Bundle {
  val isInstruction = Bool()
  val address = UInt(64.W)
  val burstLength = UInt(8.W)
  val size = new MemoryAccessWidth.Type()
  val signed = Bool()
  val outputTag = new Tag()
}

object MemoryReadTransaction {
  def ReadToTag(
    address: UInt,
    size: MemoryAccessWidth.Type,
    signed: Bool,
    outputTag: Tag
  )(implicit params: Parameters): MemoryReadTransaction = {
    val w = Wire(new MemoryReadTransaction)
    w.isInstruction := false.B
    w.address := address
    w.burstLength := 0.U
    w.size := size
    w.signed := signed
    w.outputTag := outputTag
    w
  }

  def ReadInstruction(address: UInt, length: Int, threadId: UInt)(implicit
    params: Parameters
  ): MemoryReadTransaction = {
    val w = Wire(new MemoryReadTransaction)
    w.isInstruction := true.B
    w.address := address
    w.burstLength := (length - 1).U
    w.size := MemoryAccessWidth.DoubleWord
    w.signed := false.B
    w.outputTag := Tag(threadId, 0.U)
    w
  }
}
