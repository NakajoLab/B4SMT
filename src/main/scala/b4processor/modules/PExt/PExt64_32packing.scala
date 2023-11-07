package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64_32packing {
  def pext64_32packing = (rs1: UInt, rs2: UInt) => {
    import b4processor.modules.PExt.PExtensionOperation._
    def PKXX32(fst: Int, snd: Int) =
      (rs1.W(fst) ## rs2.W(snd), false.B)

    Seq(
//      PKBB32 -> PKXX32(0, 0),
//      PKBT32 -> PKXX32(0, 1),
//      PKTB32 -> PKXX32(1, 0),
//      PKTT32 -> PKXX32(1, 1)
    )
  }
}
