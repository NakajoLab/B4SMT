package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64_Q15 {
  def pextQ15(rs1: UInt, rs2: UInt, rd: UInt) = {
    def KHMXX16(fst: Int, snd: Int) = {
      val out = Seq.fill(2)(Wire(UInt(32.W)))
      var overflow = false.B
      for (x <- 0 until 2) {
        val t = rs1.W(x).H(fst).asSInt * rs2.W(x).H(snd).asSInt
        val res = SAT.Q15((t >> 15).asUInt)
        overflow = overflow | res._2
        out(x) := res._1
      }
      (out.reverse.reduce(_ ## _), overflow)
    }

    def KDMXX16(fst: Int, snd: Int) = {
      val out = Seq.fill(2)(Wire(UInt(32.W)))
      var overflow = false.B
      for (x <- 0 until 2) {
        val t = rs1.W(x).H(fst).asSInt * rs2.W(x).H(snd).asSInt
        val res = SAT.Q31((t << 1).asUInt)
        overflow = overflow | res._2
        out(x) := res._1
      }
      (out.reverse.reduce(_ ## _), overflow)
    }

    def KDMAXX16(fst: Int, snd: Int) = {
      val out = Seq.fill(2)(Wire(UInt(32.W)))
      var overflow = false.B
      for (x <- 0 until 2) {
        val t = rs1.W(x).H(fst).asSInt * rs2.W(x).H(snd).asSInt
        val res = SAT.Q31((t << 1).asUInt)
        overflow = overflow | res._2
        val res2 = SAT.Q31(rd.W(x) + res._1)
        overflow = overflow | res2._2
        out(x) := res2._1
      }
      (out.reverse.reduce(_ ## _), overflow)
    }

    Seq(
      KHMBB16 -> KHMXX16(0, 0),
      KHMBT16 -> KHMXX16(0, 1),
      KHMTT16 -> KHMXX16(1, 1),
      KDMBB16 -> KDMXX16(0, 0),
      KDMBT16 -> KDMXX16(0, 1),
      KDMTT16 -> KDMXX16(1, 1),
      KDMABB16 -> KDMAXX16(0, 0),
      KDMABT16 -> KDMAXX16(0, 1),
      KDMATT16 -> KDMAXX16(1, 1),
    )
  }

}
