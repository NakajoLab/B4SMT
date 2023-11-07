package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExtSigned16MulWith64AddSub {
  def pextSigned16MulWith32AddSub(rs1: UInt, rs2: UInt, rd: UInt) = {
    def SMALXX(fst: Boolean, snd: Boolean) = {
      val m0 = rs1.W(0).H(if (fst) 1 else 0).asSInt * rs2
        .W(0)
        .H(if (snd) 1 else 0)
        .asSInt
      val m1 = rs1.W(1).H(if (fst) 1 else 0).asSInt * rs2
        .W(1)
        .H(if (snd) 1 else 0)
        .asSInt
      ((rd.asSInt + m0 + m1).asUInt, false.B)
    }

    Seq(
      SMALBB -> SMALXX(false, false),
      SMALBT -> SMALXX(false, true),
      SMALTT -> SMALXX(true, true),
      SMALDA -> {
        val m0 = rs1.W(0).H(0).asSInt * rs2.W(0).H(0).asSInt
        val m1 = rs1.W(0).H(1).asSInt * rs2.W(0).H(1).asSInt
        val m2 = rs1.W(1).H(0).asSInt * rs2.W(1).H(0).asSInt
        val m3 = rs1.W(1).H(1).asSInt * rs2.W(1).H(1).asSInt
        ((rd.asSInt + m0 + m1 + m2 + m3).asUInt, false.B)
      },
      SMALXDA -> {
        val m0 = rs1.W(0).H(0).asSInt * rs2.W(0).H(1).asSInt
        val m1 = rs1.W(0).H(1).asSInt * rs2.W(0).H(0).asSInt
        val m2 = rs1.W(1).H(0).asSInt * rs2.W(1).H(1).asSInt
        val m3 = rs1.W(1).H(1).asSInt * rs2.W(1).H(0).asSInt
        ((rd.asSInt + m0 + m1 + m2 + m3).asUInt, false.B)
      },
      SMALDS -> {
        val m0 = rs1.W(0).H(1).asSInt * rs2.W(0).H(1).asSInt
        val m1 = rs1.W(0).H(0).asSInt * rs2.W(0).H(0).asSInt
        val m2 = rs1.W(1).H(1).asSInt * rs2.W(1).H(1).asSInt
        val m3 = rs1.W(1).H(0).asSInt * rs2.W(1).H(0).asSInt
        ((rd.asSInt + m0 - m1 + m2 - m3).asUInt, false.B)
      },
      SMALDRS -> {
        val m0 = rs1.W(0).H(0).asSInt * rs2.W(0).H(0).asSInt
        val m1 = rs1.W(0).H(1).asSInt * rs2.W(0).H(1).asSInt
        val m2 = rs1.W(1).H(0).asSInt * rs2.W(1).H(0).asSInt
        val m3 = rs1.W(1).H(1).asSInt * rs2.W(1).H(1).asSInt
        ((rd.asSInt + m0 - m1 + m2 - m3).asUInt, false.B)
      },
      SMALXDS -> {
        val m0 = rs1.W(0).H(1).asSInt * rs2.W(0).H(0).asSInt
        val m1 = rs1.W(0).H(0).asSInt * rs2.W(0).H(1).asSInt
        val m2 = rs1.W(1).H(1).asSInt * rs2.W(1).H(0).asSInt
        val m3 = rs1.W(1).H(0).asSInt * rs2.W(1).H(1).asSInt
        ((rd.asSInt + m0 - m1 + m2 - m3).asUInt, false.B)
      },
      SMSLDA -> {
        val m0 = rs1.W(0).H(1).asSInt * rs2.W(0).H(0).asSInt
        val m1 = rs1.W(0).H(0).asSInt * rs2.W(0).H(1).asSInt
        val m2 = rs1.W(1).H(1).asSInt * rs2.W(1).H(0).asSInt
        val m3 = rs1.W(1).H(0).asSInt * rs2.W(1).H(1).asSInt
        ((rd.asSInt + m0 - m1 + m2 - m3).asUInt, false.B)
      },
      SMSLXDA -> {
        val m0 = rs1.W(0).H(0).asSInt * rs2.W(0).H(1).asSInt
        val m1 = rs1.W(0).H(1).asSInt * rs2.W(0).H(0).asSInt
        val m2 = rs1.W(1).H(0).asSInt * rs2.W(1).H(1).asSInt
        val m3 = rs1.W(1).H(1).asSInt * rs2.W(1).H(0).asSInt
        ((rd.asSInt + m0 - m1 + m2 - m3).asUInt, false.B)
      },
    )
  }

}
