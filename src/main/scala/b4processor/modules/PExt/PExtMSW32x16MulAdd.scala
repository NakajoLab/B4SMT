package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExtMSW32x16MulAdd {
  def pextMsw32x16 = (rs1: UInt, rs2: UInt, rd: UInt) =>
    Seq(
      // signed
      SMMWB -> SMMWXx(false, false),
      SMMWB_U -> SMMWXx(false, true),
      SMMWT -> SMMWXx(true, false),
      SMMWT_U -> SMMWXx(true, true),
      KMMAWB -> KMMAWXx(false, false),
      KMMAWB_U -> KMMAWXx(false, true),
      KMMAWT -> KMMAWXx(true, false),
      KMMAWT_U -> KMMAWXx(true, true),
      KMMWB2 -> KMMWX2x(false, false),
      KMMWB2_U -> KMMWX2x(false, true),
      KMMWT2 -> KMMWX2x(true, false),
      KMMWT2_U -> KMMWX2x(true, true),
      KMMAWB2 -> KMMAWX2x(false, false),
      KMMAWB2_U -> KMMAWX2x(false, true),
      KMMAWT2 -> KMMAWX2x(true, false),
      KMMAWT2_U -> KMMAWX2x(true, true),
    ).map(a => a._1 -> a._2(rs1, rs2, rd))

  def SMMWXx(top: Boolean, round: Boolean) =
    (rs1: UInt, rs2: UInt, _rd: UInt) => {
      val out = Seq.fill(2)(Wire(UInt(32.W)))
      for (x <- 0 until 2) {
        val a = rs1.W(x)
        val b = rs2.W(x)
        val mul48 = a.asSInt * (b.H(if (top) 1 else 0).asSInt)
        val t = if (round) ROUND(mul48(47, 16), mul48(15)) else mul48(47, 16)
        out(x) := t
      }
      (out.reverse.reduce(_ ## _), false.B)
    }

  def KMMAWXx(top: Boolean, round: Boolean) =
    (rs1: UInt, rs2: UInt, rd: UInt) => {
      val out = Seq.fill(2)(Wire(UInt(32.W)))
      var overflow = false.B
      for (x <- 0 until 2) {
        val a = rs1.W(x)
        val b = rs2.W(x)
        val mul48 = a.asSInt * (b.H(if (top) 1 else 0).asSInt)
        val t = if (round) ROUND(mul48(47, 16), mul48(15)) else mul48(47, 16)
        val out_t = SAT.Q31(rd.W(x) + t)
        out(x) := out_t._1
        overflow = overflow | out_t._2
      }
      (out.reverse.reduce(_ ## _), overflow)
    }

  def KMMWX2x(top: Boolean, round: Boolean) =
    (rs1: UInt, rs2: UInt, _rd: UInt) => {

      val out = Seq.fill(2)(Wire(UInt(32.W)))
      var overflow = false.B
      for (x <- 0 until 2) {
        val a = rs1.W(x)
        val b = rs2.W(x)
        val cond = a === "x80000000".U && b.H(if (top) 1 else 0) === "x8000".U
        when(cond) {
          out(x) := "x7fffffff".U
        }.otherwise {
          val mul48 = a.asSInt * (b.H(if (top) 1 else 0).asSInt)
          val shifted = (mul48 << 1).asUInt
          val t =
            if (round) ROUND(shifted(47, 16), shifted(15)) else shifted(47, 16)
          out(x) := t
        }
        overflow = overflow | cond
      }
      (out.reverse.reduce(_ ## _), overflow)
    }

  def KMMAWX2x(top: Boolean, round: Boolean) =
    (rs1: UInt, rs2: UInt, rd: UInt) => {
      val out = Seq.fill(2)(Wire(UInt(32.W)))
      var overflow = false.B
      for (x <- 0 until 2) {
        val a = rs1.W(x)
        val b = rs2.W(x)
        val cond = a === "x80000000".U && b.H(if (top) 1 else 0) === "x8000".U
        val t = Wire(UInt(32.W))
        when(cond) {
          t := "x7fffffff".U
        }.otherwise {
          val mul48 = a.asSInt * (b.H(if (top) 1 else 0).asSInt)
          val shifted = (mul48 << 1).asUInt
          t := (if (round) ROUND(shifted(47, 16), shifted(15))
                else shifted(47, 16))

        }
        val out_t = SAT.Q31(rd.W(x) + t)
        out(x) := out_t._1
        overflow = overflow | Mux(cond, true.B, out_t._2)
      }
      (out.reverse.reduce(_ ## _), overflow)
    }
}
