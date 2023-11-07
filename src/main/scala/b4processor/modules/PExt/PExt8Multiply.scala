package b4processor.modules.PExt

import b4processor.modules.PExt.PExt16AddSub.DirectionType
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt8Multiply {
  sealed trait MulProcess

  object MulProcess {
    case object SignedMultiply extends MulProcess
    case object SignedCrossedMultiply extends MulProcess

    case object UnsignedMultiply extends MulProcess
    case object UnsignedCrossedMultiply extends MulProcess

    case object SaturatingMultiply extends MulProcess
    case object SaturatingCrossedMultiply extends MulProcess
  }

  def pext8mul = {
    import PExtensionOperation._
    import b4processor.modules.PExt.PExt8Multiply.MulProcess._
    Seq(
      SMUL8 -> processMul8(SignedMultiply),
      SMULX8 -> processMul8(SignedCrossedMultiply),
      UMUL8 -> processMul8(UnsignedMultiply),
      UMULX8 -> processMul8(UnsignedCrossedMultiply),
      KHM8 -> processMul8(SaturatingMultiply),
      KHMX8 -> processMul8(SaturatingCrossedMultiply),
    )
  }

  def processMul8(ptype: MulProcess) =
    (rs1: UInt, rs2: UInt) => {
      val out = Seq.fill(4)(Wire(UInt(16.W)))
      var overflow = false.B

      import MulProcess._
      (ptype match {
        case SignedMultiply => {
          for (x <- 0 until 4)
            out(x) := (rs1.B(x).asSInt * rs2.B(x).asSInt).asUInt
        }
        case SignedCrossedMultiply => {
          for (x <- Seq(1, 3)) {
            out(x) := (rs1.B(x).asSInt * rs2.B(x - 1).asSInt).asUInt
            out(x - 1) := (rs1.B(x - 1).asSInt * rs2.B(x).asSInt).asUInt
          }
        }
        case UnsignedMultiply => {
          for (x <- 0 until 4)
            out(x) := rs1.B(x) * rs2.B(x)
        }
        case UnsignedCrossedMultiply => {
          for (x <- Seq(1, 3)) {
            out(x) := (rs1.B(x) * rs2.B(x - 1))
            out(x - 1) := (rs1.B(x - 1) * rs2.B(x))
          }
        }
        case SaturatingMultiply => {
          val outt = Seq.fill(8)(Wire(UInt(8.W)))
          for (x <- 0 until 8) {
            val t = rs1.B(x).asSInt * rs2.B(x).asSInt
            val out_t = SAT.Q7((t >> 7).asUInt)
            outt(x) := out_t._1
            overflow = overflow | out_t._2
          }
          out(0) := outt(1) ## outt(0)
          out(1) := outt(3) ## outt(2)
          out(2) := outt(5) ## outt(4)
          out(3) := outt(7) ## outt(6)
        }
        case SaturatingCrossedMultiply => {
          val outt = Seq.fill(8)(Wire(UInt(8.W)))
          for (
            (x, y) <- Seq(
              (7, 6),
              (6, 7),
              (5, 4),
              (4, 5),
              (3, 2),
              (2, 3),
              (1, 0),
              (0, 1),
            )
          ) {
            val t = rs1.B(x).asSInt * rs2.B(y).asSInt
            val out_t = SAT.Q7((t >> 7).asUInt)
            outt(x) := out_t._1
            overflow = overflow | out_t._2
          }
          out(0) := outt(1) ## outt(0)
          out(1) := outt(3) ## outt(2)
          out(2) := outt(5) ## outt(4)
          out(3) := outt(7) ## outt(6)
        }
      })
      (out.reverse.reduce(_ ## _), overflow)
    }
}
