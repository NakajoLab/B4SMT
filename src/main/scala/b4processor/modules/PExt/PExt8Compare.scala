package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt8Compare {
  sealed trait CmpProcess

  object CmpProcess {
    case object Equal extends CmpProcess

    case object SignedLessThan extends CmpProcess

    case object SignedLessThanEqual extends CmpProcess

    case object UnsignedLessThan extends CmpProcess

    case object UnsignedLessThanEqual extends CmpProcess
  }

  def pext8cmp = {
    import PExtensionOperation._
    import b4processor.modules.PExt.PExt8Compare.CmpProcess._
    Seq(
      CMPEQ8 -> processCmp8(Equal),
      SCMPLT8 -> processCmp8(SignedLessThan),
      SCMPLE8 -> processCmp8(SignedLessThanEqual),
      UCMPLT8 -> processCmp8(UnsignedLessThan),
      UCMPLE8 -> processCmp8(UnsignedLessThanEqual),
    )
  }

  def processCmp8(ptype: CmpProcess) =
    (rs1: UInt, rs2: UInt) => {
      val out = Seq.fill(8)(Wire(UInt(8.W)))
      for (x <- 0 until 8) {

        import CmpProcess._
        val t = Wire(UInt(8.W))
        t := (ptype match {
          case Equal => Mux(rs1.B(x) === rs2.B(x), "xff".U, 0.U)
          case SignedLessThan =>
            Mux(rs1.B(x).asSInt < rs2.B(x).asSInt, "xff".U, 0.U)
          case SignedLessThanEqual =>
            Mux(rs1.B(x).asSInt <= rs2.B(x).asSInt, "xff".U, 0.U)
          case UnsignedLessThan => Mux(rs1.B(x) < rs2.B(x), "xff".U, 0.U)
          case UnsignedLessThanEqual =>
            Mux(rs1.B(x) <= rs2.B(x), "xff".U, 0.U)

        })
        out(x) := t
      }
      (out.reverse.reduce(_ ## _), false.B)
    }
}
