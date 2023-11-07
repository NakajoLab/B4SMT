package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExtQ16Saturate {
  def pextQ16Saturate(rs1: UInt, rs2: UInt, rd: UInt) = {
    Seq(
      KADDH -> {
        val a = SE17(rs1.H(0))
        val b = SE17(rs1.H(0))
        val t = a + b
        val k = SAT.Q15(t)
        val rd = SE(64)(k._1)
        val overflow = k._2
        (rd, overflow)
      },
      KSUBH -> {
        val a = SE17(rs1.H(0))
        val b = SE17(rs1.H(0))
        val t = a - b
        val k = SAT.Q15(t)
        val rd = SE(64)(k._1)
        val overflow = k._2
        (rd, overflow)
      },
      KHMBB -> {
        val a = rs1.H(0).asSInt
        val b = rs1.H(0).asSInt
        val t = a * b
        SAT.Q15((t >> 15).asUInt)
      },
      KHMBT -> {
        val a = rs1.H(0).asSInt
        val b = rs1.H(1).asSInt
        val t = a * b
        SAT.Q15((t >> 15).asUInt)
      },
      KHMTT -> {
        val a = rs1.H(1).asSInt
        val b = rs1.H(1).asSInt
        val t = a * b
        SAT.Q15((t >> 15).asUInt)
      },
      UKADDH -> {
        val a = ZE17(rs1.H(0))
        val b = ZE17(rs2.H(0))
        val t = a + b
        val uk16 = SAT.U16(t)
        (SE(64)(uk16._1), uk16._2)
      },
      UKSUBH -> {
        val a = ZE17(rs1.H(0))
        val b = ZE17(rs2.H(0))
        val t = a - b
        val uk16 = SAT.U16(t)
        (SE(64)(uk16._1), uk16._2)
      },
    )
  }

}
