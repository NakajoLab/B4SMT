package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt16Compare {
  sealed trait CmpProcess

  object CmpProcess {
    case object Equal extends CmpProcess
    case object SignedLessThan extends CmpProcess
    case object SignedLessThanEqual extends CmpProcess
    case object UnsignedLessThan extends CmpProcess
    case object UnsignedLessThanEqual extends CmpProcess
  }

  def pext16cmp = {
    import PExtensionOperation._
    import b4processor.modules.PExt.PExt16Compare.CmpProcess._
    Seq(
      CMPEQ16 -> processCmp16(Equal),
      SCMPLT16 -> processCmp16(SignedLessThan),
      SCMPLE16 -> processCmp16(SignedLessThanEqual),
      UCMPLT16 -> processCmp16(UnsignedLessThan),
      UCMPLE16 -> processCmp16(UnsignedLessThanEqual),
    )
  }

  def processCmp16(ptype: CmpProcess) =
    (rs1: UInt, rs2: UInt) => {
      val out = Seq.fill(4)(Wire(UInt(16.W)))
      for (x <- 0 until 4) {

        import CmpProcess._
        val t = Wire(UInt(16.W))
        t := (ptype match {
          case Equal => Mux(rs1.H(x) === rs2.H(x), "xffff".U, 0.U)
          case SignedLessThan =>
            Mux(rs1.H(x).asSInt < rs2.H(x).asSInt, "xffff".U, 0.U)
          case SignedLessThanEqual =>
            Mux(rs1.H(x).asSInt <= rs2.H(x).asSInt, "xffff".U, 0.U)
          case UnsignedLessThan => Mux(rs1.H(x) < rs2.H(x), "xffff".U, 0.U)
          case UnsignedLessThanEqual =>
            Mux(rs1.H(x) <= rs2.H(x), "xffff".U, 0.U)

        })
        out(x) := t
      }
      (out.reverse.reduce(_ ## _), false.B)
    }
}
