package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExtMisc {
  def pextMisc(rs1: UInt, rs2: UInt, rd: UInt, imm: UInt) =
    Seq(
      SCLIP32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        val n = imm(4, 0)
        var overflow = false.B
        for (x <- 0 until 2) {
          val out_t = SAT.Q(n)(rs1.W(x))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      UCLIP32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        val m = imm(4, 0)
        var overflow = false.B
        for (x <- 0 until 2) {
          val out_t = SAT.U(m)(rs1.W(x))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      CLRS32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          out(x) := CLRS(rs1.W(x))
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      CLZ32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          out(x) := CLZ(rs1.W(x))
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      PBSAD -> {
        val d = Seq.fill(8)(Wire(UInt(64.W)))
        for (x <- 0 until 8) {
          d(x) := ABS(rs1.B(x) - rs2.B(x))
        }
        (d.reverse.reduce(_ + _), false.B)
      },
      PBSADA -> {
        val d = Seq.fill(8)(Wire(UInt(64.W)))
        for (x <- 0 until 8) {
          d(x) := ABS(rs1.B(x) - rs2.B(x))
        }
        (rd + d.reverse.reduce(_ + _), false.B)
      },
    )
}
