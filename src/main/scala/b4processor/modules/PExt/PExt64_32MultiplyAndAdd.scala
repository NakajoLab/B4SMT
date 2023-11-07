package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64_32MultiplyAndAdd {
  def pext64_32MultiplyAndAdd = (rs1: UInt, rs2: UInt, rd: UInt) => {
    import b4processor.modules.PExt.PExtensionOperation._
    def processMul64(fst: Int, snd: Int) = {
      SAT.Q63((rd.asSInt + rs1.W(fst).asSInt * rs2.W(snd).asSInt).asUInt)
    }

    Seq(
      KMABB32 -> processMul64(0, 0),
      KMABT32 -> processMul64(0, 1),
      KMATT32 -> processMul64(1, 1),
    )
  }
}
