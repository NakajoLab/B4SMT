package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64_NonSIMD32Shift {
  def pext64_NonSIMD32Shift = (rs1: UInt, imm: UInt) => {
    import b4processor.modules.PExt.PExtensionOperation._

    Seq(SRAIW_U -> {
      val amt = imm(4, 0)
      val res =
        Mux(
          amt === 0.U,
          rs1.W(0),
          (SE33((rs1.W(0) >> (amt - 1.U)).asUInt) + 1.U)(32, 1),
        )
      (SE64(res), false.B)
    })
  }
}
