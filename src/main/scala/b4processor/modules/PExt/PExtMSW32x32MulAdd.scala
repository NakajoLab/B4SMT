package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper.SAT.Q31
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExtMSW32x32MulAdd {
  def pextMsw32x32 = (rs1: UInt, rs2: UInt, rd: UInt) =>
    Seq(
      // signed
      SMMUL -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          val t64 = (rs1.W(x).asSInt * rs2.W(x).asSInt).asUInt
          out(x) := t64.W(1)
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      SMMUL_U -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          val t64 = (rs1.W(x).asSInt * rs2.W(x).asSInt).asUInt
          out(x) := ROUND(t64.W(1), t64.W(0)(31))
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      KMMAC -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val t64 = (rs1.W(x).asSInt * rs2.W(x).asSInt).asUInt
          val res = rd.W(x) + t64.W(1)
          val out_t = SAT.Q31(res)
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMMAC_U -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val t64 = (rs1.W(x).asSInt * rs2.W(x).asSInt).asUInt
          val t32 = ROUND(t64.W(1), t64.W(0)(31))
          val res = rd.W(x) + t32
          val out_t = SAT.Q31(res)
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMMSB -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val t64 = (rs1.W(x).asSInt * rs2.W(x).asSInt).asUInt
          val res = rd.W(x) - t64.W(1)
          val out_t = SAT.Q31(res)
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMMSB_U -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val t64 = (rs1.W(x).asSInt * rs2.W(x).asSInt).asUInt
          val t32 = ROUND(t64.W(1), t64.W(0)(31))
          val res = rd.W(x) - t32
          val out_t = SAT.Q31(res)
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KWMMUL -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val t64 = (rs1.W(x).asSInt * rs2.W(x).asSInt).asUInt
          val s64 = SAT.Q63((t64 << 1).asUInt)
          out(x) := s64._1.W(1)
          overflow = overflow | s64._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KWMMUL_U -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val t64 = (rs1.W(x).asSInt * rs2.W(x).asSInt).asUInt
          val round = t64(63, 30) + 1.U
          val out_t = SAT.Q31(round(33, 1))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
    )
}
