package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExtQ32Saturate {
  def pextQ32Saturate(rs1: UInt, rs2: UInt, rd: UInt, imm: UInt) = {
    Seq(
      KADDW -> {
        val a = rs1.W(0)
        val b = rs1.W(0)
        val t = a + b
        val k = SAT.Q31(t)
        val rd = SE(64)(k._1)
        val overflow = k._2
        (rd, overflow)
      },
      UKADDW -> {
        val a = ZE33(rs1.W(0))
        val b = ZE33(rs1.W(0))
        val t = a + b
        val k = SAT.Q31(t)
        val rd = SE(64)(k._1)
        val overflow = k._2
        (rd, overflow)
      },
      KSUBW -> {
        val a = rs1.W(0)
        val b = rs1.W(0)
        val t = a - b
        val k = SAT.Q31(t)
        val rd = SE(64)(k._1)
        val overflow = k._2
        (rd, overflow)
      },
      UKSUBW -> {
        val a = ZE33(rs1.W(0))
        val b = ZE33(rs1.W(0))
        val t = a - b
        val k = SAT.U32(t)
        val rd = SE(64)(k._1)
        val overflow = k._2
        (rd, overflow)
      },
      KDMBB -> {
        val a = rs1.H(0).asSInt
        val b = rs1.H(0).asSInt
        val t = ((a * b) << 1).asUInt
        val k = SAT.Q31(t)
        val rd = k._1
        val overflow = k._2
        (rd, overflow)
      },
      KDMBT -> {
        val a = rs1.H(0).asSInt
        val b = rs1.H(1).asSInt
        val t = ((a * b) << 1).asUInt
        val k = SAT.Q31(t)
        val rd = k._1
        val overflow = k._2
        (rd, overflow)
      },
      KDMTT -> {
        val a = rs1.H(1).asSInt
        val b = rs1.H(1).asSInt
        val t = ((a * b) << 1).asUInt
        val k = SAT.Q31(t)
        val rd = k._1
        val overflow = k._2
        (rd, overflow)
      },
      KSLRAW -> {
        val rd = Wire(UInt(64.W))
        val overflow = WireInit(false.B)
        when(rs2(5, 0).asSInt >= 0.S) {
          val t = SAT.Q31((rs1 << rs2(5, 0)).asUInt)
          rd := t._1
          overflow := t._2
        }.otherwise {
          rd := (rs1.asSInt >> -rs2(5, 0)).asUInt
        }
        (rd, overflow)
      },
      KSLRAW_U -> {
        val rd = Wire(UInt(64.W))
        val overflow = WireInit(false.B)
        when(rs2(5, 0).asSInt >= 0.S) {
          val t = SAT.Q31((rs1 << rs2(5, 0)).asUInt)
          rd := t._1
          overflow := t._2
        }.otherwise {
          val amt = -rs2(5, 0)
          rd := ROUND((rs1.asSInt >> amt).asUInt, rs1(amt - 1.U))
        }
        (rd, overflow)
      },
      KSLLW -> {
        val w = rs1.W(0)
        val imm5 = rs2(4, 0)
        val t = SAT.Q31((w << imm5).asUInt)
        (SE64(t._1), t._2)
      },
      KSLLIW -> {
        val w = rs1.W(0)
        val imm5 = imm
        val t = SAT.Q31((w << imm5).asUInt)
        (SE64(t._1), t._2)
      },
      KDMABB -> {
        val m = ((rs1.H(0).asSInt * rs2.H(0).asSInt) << 1).asUInt
        val out_t = SAT.Q31(m)
        val res = rd.W(0) + out_t._1
        (SE64(res), out_t._2)
      },
      KDMABT -> {
        val m = ((rs1.H(0).asSInt * rs2.H(1).asSInt) << 1).asUInt
        val out_t = SAT.Q31(m)
        val res = rd.W(0) + out_t._1
        (SE64(res), out_t._2)
      },
      KDMATT -> {
        val m = ((rs1.H(1).asSInt * rs2.H(1).asSInt) << 1).asUInt
        val out_t = SAT.Q31(m)
        val res = rd.W(0) + out_t._1
        (SE64(res), out_t._2)
      },
      KABSW -> {
        val out_t = SAT.Q31(ABS(rs1.W(0)))
        (SE64(out_t._1), out_t._2)
      },
    )
  }

}
