package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._
import chisel3.util.MuxCase

object PExt8Misc {
  def pext8misc = (rs1: UInt, rs2: UInt, imm: UInt) =>
    Seq(
      SMIN8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        for (x <- 0 until 8)
          out(x) := Mux(rs1.B(x).asSInt < rs2.B(x).asSInt, rs1.B(x), rs2.B(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      UMIN8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        for (x <- 0 until 8)
          out(x) := Mux(rs1.B(x) < rs2.B(x), rs1.B(x), rs2.B(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      SMAX8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        for (x <- 0 until 8)
          out(x) := Mux(rs1.B(x).asSInt > rs2.B(x).asSInt, rs1.B(x), rs2.B(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      UMAX8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        for (x <- 0 until 8)
          out(x) := Mux(rs1.B(x) > rs2.B(x), rs1.B(x), rs2.B(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      SCLIP8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        var overflow = false.B
        for (x <- 0 until 8) {
          val out_t = SAT.Q(imm(2, 0))(rs1.B(x))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      UCLIP8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        var overflow = false.B
        for (x <- 0 until 8) {
          val out_t = SAT.U(imm(2, 0))(rs1.B(x))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KABS8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        for (x <- 0 until 8)
          out(x) := ABS(rs1.B(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      CLRS8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        for (x <- 0 until 8) {
          val snum = rs1.B(x).asSInt
          out(x) := MuxCase(
            0.U,
            (0 until 7).map(i => (snum(i) === snum(7)) -> (7 - i).U),
          )
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
      CLZ8 -> {
        val out = Seq.fill(8)(Wire(UInt(8.W)))
        for (x <- 0 until 8) {
          val snum = rs1.B(x).asSInt
          out(x) := MuxCase(
            0.U,
            (0 until 8).map(i => (snum(i) === 0.U) -> (7 - i).U),
          )
        }
        (out.reverse.reduce(_ ## _), false.B)
      },
//      SWAP8 -> {
      //        val out = Seq.fill(4)(Wire(UInt(16.W)))
      //        for (x <- 0 until 4) {
      //          val ah0 = rs1.H(x).B(0)
      //          val ah1 = rs1.H(x).B(1)
      //          out(x) := ah0 ## ah1
      //        }
      //        (out.reverse.reduce(_ ## _), false.B)
      //      }
    )

}
