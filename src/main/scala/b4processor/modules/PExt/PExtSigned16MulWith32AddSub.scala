package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExtSigned16MulWith32AddSub {
  def pextSigned16MulWith32AddSub(rs1: UInt, rs2: UInt, rd: UInt) =
    Seq(
      // signed
      SMBB16 -> SMXX16(false, false)(rs1, rs2, rd),
      SMBT16 -> SMXX16(false, true)(rs1, rs2, rd),
      SMTT16 -> SMXX16(true, true)(rs1, rs2, rd),
      KMDA -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(1).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(0).asSInt).asUInt
          val t = SAT.Q31(mul1 + mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMXDA -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(0).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(1).asSInt).asUInt
          val t = SAT.Q31(mul1 + mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      SMDS -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(1).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(0).asSInt).asUInt
          val t = mul1 - mul2
          out(x) := t
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      SMDRS -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(1).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(0).asSInt).asUInt
          val t = mul2 - mul1
          out(x) := t
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      SMXDS -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(0).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(1).asSInt).asUInt
          val t = mul1 - mul2
          out(x) := t
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      KMABB -> KMAXX(false, false)(rs1, rs2, rd),
      KMABT -> KMAXX(false, true)(rs1, rs2, rd),
      KMATT -> KMAXX(true, true)(rs1, rs2, rd),
      KMADA -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(1).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(0).asSInt).asUInt
          val t = SAT.Q31(rd.W(x) + mul1 + mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMAXDA -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(0).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(1).asSInt).asUInt
          val t = SAT.Q31(rd.W(x) + mul1 + mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMADS -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(1).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(0).asSInt).asUInt
          val t = SAT.Q31(rd.W(x) + mul1 - mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMAXDS -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(0).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(1).asSInt).asUInt
          val t = SAT.Q31(rd.W(x) + mul1 - mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMADRS -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(1).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(0).asSInt).asUInt
          val t = SAT.Q31(rd.W(x) + mul2 - mul1)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMAXDS -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(0).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(1).asSInt).asUInt
          val t = SAT.Q31(rd.W(x) + mul1 - mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMSDA -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(1).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(0).asSInt).asUInt
          val t = SAT.Q31(rd.W(x) - mul1 - mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KMSXDA -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val a = rs1.W(x)
          val b = rs2.W(x)
          val mul1 = (a.H(1).asSInt * b.H(0).asSInt).asUInt
          val mul2 = (a.H(0).asSInt * b.H(1).asSInt).asUInt
          val t = SAT.Q31(rd.W(x) - mul1 - mul2)
          out(x) := t._1
          overflow = overflow | t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
    )

  def SMXX16(fst: Boolean, snd: Boolean) = (rs1: UInt, rs2: UInt, rd: UInt) => {
    val out = Seq.fill(2)(Wire(UInt(32.W)))
    for (x <- 0 until 2) {
      val a = rs1.W(x)
      val b = rs2.W(x)
      out(x) := (a.H(if (fst) 1 else 0).asSInt *
        b.H(if (snd) 1 else 0).asSInt).asUInt
    }
    (out.reverse.reduce(_ ## _), false.B)
  }

  def KMAXX(fst: Boolean, snd: Boolean) = (rs1: UInt, rs2: UInt, rd: UInt) => {
    val out = Seq.fill(2)(Wire(UInt(32.W)))
    var overflow = false.B
    for (x <- 0 until 2) {
      val a = rs1.W(x)
      val b = rs2.W(x)
      val mul =
        (a.H(if (fst) 1 else 0).asSInt * b.H(if (snd) 1 else 0).asSInt).asUInt
      val t = SAT.Q31(rd + mul)
      out(x) := t._1
      overflow = overflow | t._2
    }
    (out.reverse.reduce(_ ## _), overflow)
  }
}
