package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64_32ParallelMultiplyAndAdd {
  def pext64_32ParallelMultiplyAndAdd = (rs1: UInt, rs2: UInt, rd: UInt) => {
    import b4processor.modules.PExt.PExtensionOperation._
    def processMul64(fst: Int, snd: Int) = {
      SAT.Q63((rd.asSInt + rs1.W(fst).asSInt * rs2.W(snd).asSInt).asUInt)
    }

    Seq(
      KMDA32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        SAT.Q63(t1.asUInt + t0.asUInt)
      },
      KMXDA32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(1).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(0).asSInt
        SAT.Q63(t1.asUInt + t0.asUInt)
      },
//      KMADA32 -> {
//        val t0 = rs1.W(0).asSInt * rs2.W(0).asSInt
//        val t1 = rs1.W(1).asSInt * rs2.W(1).asSInt
//        SAT.Q63(rd + t1.asUInt + t0.asUInt)
//      },
      KMAXDA32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(1).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(0).asSInt
        SAT.Q63(rd + t1.asUInt + t0.asUInt)
      },
      KMADS32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        SAT.Q63(rd + t1.asUInt - t0.asUInt)
      },
      KMADRS32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        SAT.Q63(rd + t0.asUInt - t1.asUInt)
      },
      KMAXDS32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(1).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(0).asSInt
        SAT.Q63(rd + t1.asUInt - t0.asUInt)
      },
      KMSDA32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        SAT.Q63(rd - t1.asUInt - t0.asUInt)
      },
      KMSXDA32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(1).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(0).asSInt
        SAT.Q63(rd - t1.asUInt - t0.asUInt)
      },
      SMDS32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        (t1.asUInt - t0.asUInt, false.B)
      },
      SMDRS32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(0).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(1).asSInt
        (t0.asUInt - t1.asUInt, false.B)
      },
      SMXDS32 -> {
        val t0 = rs1.W(0).asSInt * rs2.W(1).asSInt
        val t1 = rs1.W(1).asSInt * rs2.W(0).asSInt
        (t1.asUInt - t0.asUInt, false.B)
      },
    )
  }
}
