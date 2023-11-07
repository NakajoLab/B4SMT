package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt8MulWith32Add {
  def pext8MulWith32Add(rs1: UInt, rs2: UInt, rd: UInt, imm: UInt) =
    Seq(
      SMAQA -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val m = Seq.fill(4)(Wire(SInt(32.W)))
          for (i <- 0 until 4) {
            m(i) := a.B(i).asSInt * b.B(i).asSInt
          }
          out(x) := (rd.W(x).asSInt + m.reduce(_ + _)).asUInt
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      UMAQA -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val m = Seq.fill(4)(Wire(UInt(32.W)))
          for (i <- 0 until 4) {
            m(i) := a.B(i) * b.B(i)
          }
          out(x) := (rd.W(x) + m.reduce(_ + _))
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      SMAQASU -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val m = Seq.fill(4)(Wire(SInt(32.W)))
          for (i <- 0 until 4) {
            m(i) := a.B(i).asSInt * b.B(i)
          }
          out(x) := (rd.W(x).asSInt + m.reduce(_ + _)).asUInt
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
    )
}
