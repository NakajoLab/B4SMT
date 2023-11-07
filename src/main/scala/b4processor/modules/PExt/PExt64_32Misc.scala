package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._
import chisel3.util.MuxCase

object PExt64_32Misc {
  def pext32misc = (rs1: UInt, rs2: UInt, imm: UInt) =>
    Seq(
      SMIN32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2)
          out(x) := Mux(rs1.W(x).asSInt < rs2.W(x).asSInt, rs1.W(x), rs2.W(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      UMIN32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2)
          out(x) := Mux(rs1.W(x) < rs2.W(x), rs1.W(x), rs2.W(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      SMAX32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2)
          out(x) := Mux(rs1.W(x).asSInt > rs2.W(x).asSInt, rs1.W(x), rs2.W(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      UMAX32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2)
          out(x) := Mux(rs1.W(x) > rs2.W(x), rs1.W(x), rs2.W(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
      SCLIP32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val out_t = SAT.Q(imm(3, 0))(rs1.W(x))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      UCLIP32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        var overflow = false.B
        for (x <- 0 until 2) {
          val out_t = SAT.U(imm)(rs1.W(x))
          out(x) := out_t._1
          overflow = overflow | out_t._2
        }
        (out.reverse.reduce(_ ## _), overflow)
      },
      KABS32 -> {
        val out = Seq.fill(2)(Wire(UInt(32.W)))
        for (x <- 0 until 2)
          out(x) := ABS(rs1.W(x))
        (out.reverse.reduce(_ ## _), false.B)
      },
    )

}
