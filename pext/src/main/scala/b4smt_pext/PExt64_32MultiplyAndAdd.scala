package b4smt_pext

import chisel3._
import PExtensionOperation._
import UIntSectionHelper._

object PExt64_32MultiplyAndAdd {
  def pext64_32MultiplyAndAdd = (rs1: UInt, rs2: UInt, rd: UInt) => {
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
