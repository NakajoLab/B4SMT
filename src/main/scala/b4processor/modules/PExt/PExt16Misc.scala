package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._
import chisel3.util.MuxCase

object PExt16Misc {
  def pext16misc = (rs1: UInt, rs2: UInt, imm: UInt) =>
    Seq(
      SMIN16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        for (x <- 0 until 4)
          out(x) := Mux(rs1.H(x).asSInt < rs2.H(x).asSInt, rs1.H(x), rs2.H(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      UMIN16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        for (x <- 0 until 4)
          out(x) := Mux(rs1.H(x) < rs2.H(x), rs1.H(x), rs2.H(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      SMAX16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        for (x <- 0 until 4)
          out(x) := Mux(rs1.H(x).asSInt > rs2.H(x).asSInt, rs1.H(x), rs2.H(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      UMAX16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        for (x <- 0 until 4)
          out(x) := Mux(rs1.H(x) > rs2.H(x), rs1.H(x), rs2.H(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      SCLIP16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        var overflow = false.B
        for (x <- 0 until 4) {
          val out_t = SAT.Q(imm(3, 0))(rs1.H(x))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      UCLIP16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        var overflow = false.B
        for (x <- 0 until 4) {
          val out_t = SAT.U(imm)(rs1.H(x))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KABS16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        for (x <- 0 until 4)
          out(x) := ABS(rs1.H(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      CLRS16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        for (x <- 0 until 4) {
          val snum = rs1.H(x).asSInt
          out(x) := MuxCase(
            0.U,
            (0 until 15).map(i => (snum(i) === snum(15)) -> (15 - i).U),
          )
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      CLZ16 -> {
        val out = Seq.fill(4)(Wire(UInt(16.W)))
        for (x <- 0 until 4) {
          val snum = rs1.H(x).asSInt
          out(x) := MuxCase(
            0.U,
            (0 until 16).map(i => (snum(i) === 0.U) -> (15 - i).U),
          )
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
//      SWAP16 -> {
//        val out = Seq.fill(2)(Wire(UInt(32.W)))
//        for (x <- 0 until 2) {
//          val ah0 = rs1.W(x).H(0)
//          val ah1 = rs1.W(x).H(1)
//          out(x) := ah0 ## ah1
//        }
//        (out.reverse.reduce(_ ## _), false.B)
//      }
    )

}
