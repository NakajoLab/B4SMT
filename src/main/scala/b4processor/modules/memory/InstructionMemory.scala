package b4processor.modules.memory

import b4processor.Parameters
import b4processor.connections.InstructionMemory2Cache
import chisel3._
import chisel3.util._

/** 命令メモリ */
class InstructionMemory(memoryInit: => Seq[UInt])(implicit params: Parameters)
    extends Module {

  /** キャッシュへの接続 */
  val io = IO(new InstructionMemory2Cache)

  /** メモリ本体 */
  val memory = VecInit(memoryInit)

  for (i <- 0 until params.fetchWidth) {
    // FIXME: 下位28bitしか命令のアドレスとして使えないのでmax 265MBのROMしか入れられない
    // 初期アドレスから減産してもいいが、減算の回路は規模が大きい？
    val address = io.address.asUInt(27, 0)
    io.output(i) := Cat(
      memory(address + (i * 4 + 3).U),
      memory(address + (i * 4 + 2).U),
      memory(address + (i * 4 + 1).U),
      memory(address + (i * 4).U),
    )
  }
}
