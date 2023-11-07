package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64DataComputation {
  def pext64DataComputation(rs1: UInt, rs2: UInt) =
    Seq(
      RADD64 -> (((rs1.asSInt + rs2.asSInt) >> 1).asUInt, false.B),
      URADD64 -> (((rs1 + rs2) >> 1).asUInt, false.B),
      KADD64 -> SAT.Q63(rs1 + rs2),
      UKADD64 -> SAT.U64(rs1 + rs2),
      RSUB64 -> (((rs1.asSInt - rs2.asSInt) >> 1).asUInt, false.B),
      URSUB64 -> (((rs1 - rs2) >> 1).asUInt, false.B),
      KSUB64 -> SAT.Q63(rs1 - rs2),
      UKSUB64 -> SAT.U64(rs1 - rs2),
    )
}
