package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt32Computation {
  def pext32Computation(rs1: UInt, rs2: UInt, rd: UInt, imm: UInt) = {
    Seq(
      RADDW -> {
        val res = ((rs1.W(0) + rs2.W(0)).asSInt >> 1).asUInt
        (SE64(res), false.B)
      },
      URADDW -> {
        val a = ZE33(rs1.W(0))
        val b = ZE33(rs2.W(0))
        val res = ((a + b) >> 1).asUInt
        (SE64(res), false.B)
      },
      RSUBW -> {
        val res = ((rs1.W(0) - rs2.W(0)).asSInt >> 1).asUInt
        (SE64(res), false.B)
      },
      URSUBW -> {
        val a = ZE33(rs1.W(0))
        val b = ZE33(rs2.W(0))
        val res = ((a - b) >> 1).asUInt
        (SE64(res), false.B)
      },
      MULR64 -> (rs1.W(0) * rs2.W(0), false.B),
      MULSR64 -> ((rs1.W(0).asSInt * rs2.W(0).asSInt).asUInt, false.B),
      MSUBR32 -> {
        val mres = rs1.W(0) * rs2.W(0)
        val tres = rd.W(0) - mres.W(0)
        (SE64(tres), false.B)
      },
    )
  }

}
