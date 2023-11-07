package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt32MulWith64AddSub {
  def pextMsw32x16 = (rs1: UInt, rs2: UInt, rd: UInt) =>
    Seq(
      SMAR64 -> {
        val m0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val m1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        ((rd.asSInt + m0 + m1).asUInt, false.B)
      },
      SMSR64 -> {
        val m0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val m1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        ((rd.asSInt - m0 - m1).asUInt, false.B)
      },
      UMAR64 -> {
        val m0 = rs1.W(0) * rs2.W(0)
        val m1 = rs1.W(1) * rs2.W(1)
        (rd + m0 + m1, false.B)
      },
      UMSR64 -> {
        val m0 = rs1.W(0) * rs2.W(0)
        val m1 = rs1.W(1) * rs2.W(1)
        (rd - m0 - m1, false.B)
      },
      KMAR64 -> {
        val m0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val m1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        SAT.Q63((rd.asSInt + m0 + m1).asUInt)
      },
      KMSR64 -> {
        val m0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val m1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        SAT.Q63((rd.asSInt - m0 - m1).asUInt)
      },
      UKMAR64 -> {
        val m0 = rs1.W(0) * rs2.W(0)
        val m1 = rs1.W(1) * rs2.W(1)
        SAT.U64(rd + m0 + m1)
      },
      UKMSR64 -> {
        val m0 = rs1.W(0) * rs2.W(0)
        val m1 = rs1.W(1) * rs2.W(1)
        SAT.U64(rd - m0 - m1)
      },
    )
}
