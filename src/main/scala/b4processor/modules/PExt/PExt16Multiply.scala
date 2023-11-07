package b4processor.modules.PExt

import b4processor.modules.PExt.PExt16AddSub.DirectionType
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt16Multiply {
  sealed trait MulProcess

  object MulProcess {
    case object SignedMultiply extends MulProcess
    case object SignedCrossedMultiply extends MulProcess

    case object UnsignedMultiply extends MulProcess
    case object UnsignedCrossedMultiply extends MulProcess

    case object SaturatingMultiply extends MulProcess
    case object SaturatingCrossedMultiply extends MulProcess
  }

  def pext16mul = {
    import PExtensionOperation._
    import b4processor.modules.PExt.PExt16Multiply.MulProcess._
    Seq(
      SMUL16 -> processMul16(SignedMultiply),
      SMULX16 -> processMul16(SignedCrossedMultiply),
      UMUL16 -> processMul16(UnsignedMultiply),
      UMULX16 -> processMul16(UnsignedCrossedMultiply),
      KHM16 -> processMul16(SaturatingMultiply),
      KHMX16 -> processMul16(SaturatingCrossedMultiply),
    )
  }

  def processMul16(ptype: MulProcess) =
    (rs1: UInt, rs2: UInt) => {
      val out = Seq.fill(2)(Wire(UInt(32.W)))
      var overflow = false.B
      import MulProcess._
      (ptype match {
        case SignedMultiply => {
          out(0) := (rs1.H(0).asSInt * rs2.H(0).asSInt).asUInt
          out(1) := (rs1.H(1).asSInt * rs2.H(1).asSInt).asUInt
        }
        case SignedCrossedMultiply => {
          out(0) := (rs1.H(0).asSInt * rs2.H(1).asSInt).asUInt
          out(1) := (rs1.H(1).asSInt * rs2.H(0).asSInt).asUInt
        }
        case UnsignedMultiply => {
          out(0) := rs1.H(0) * rs2.H(0)
          out(1) := rs1.H(1) * rs2.H(1)
        }
        case UnsignedCrossedMultiply => {
          out(0) := rs1.H(0) * rs2.H(1)
          out(1) := rs1.H(1) * rs2.H(0)
        }
        case SaturatingMultiply => {
          val outt = Seq.fill(4)(Wire(UInt(16.W)))
          for (x <- 0 until 4) {
            val t = rs1.H(x).asSInt * rs2.H(x).asSInt
            val out_t = SAT.Q15((t >> 15).asUInt)
            outt(x) := out_t._1
            overflow = overflow | out_t._2
          }
          out(0) := outt(1) ## outt(0)
          out(1) := outt(3) ## outt(2)
        }
        case SaturatingCrossedMultiply => {
          val outt = Seq.fill(4)(Wire(UInt(16.W)))
          for ((x, y) <- Seq((3, 2), (2, 3), (1, 0), (0, 1))) {
            val t = rs1.H(x).asSInt * rs2.H(y).asSInt
            val out_t = SAT.Q15((t >> 15).asUInt)
            outt(x) := out_t._1
            overflow = overflow | out_t._2
          }
          out(0) := outt(1) ## outt(0)
          out(1) := outt(3) ## outt(2)
        }
      })
      (out.reverse.reduce(_ ## _), overflow)
    }
}
