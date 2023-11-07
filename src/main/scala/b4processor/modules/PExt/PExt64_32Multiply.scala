package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64_32Multiply {
  sealed trait MulProcess

  object MulProcess {
    case object SignedMultiply extends MulProcess
    case object SignedCrossedMultiply extends MulProcess

    case object UnsignedMultiply extends MulProcess
    case object UnsignedCrossedMultiply extends MulProcess

    case object SaturatingMultiply extends MulProcess
    case object SaturatingCrossedMultiply extends MulProcess
  }

  def pext32mul = (rs1: UInt, rs2: UInt) => {
    import b4processor.modules.PExt.PExtensionOperation._
    def processMul64(fst: Int, snd: Int) = {
      ((rs1.W(fst).asSInt * rs2.W(snd).asSInt).asUInt, false.B)
    }
    Seq(
//      SMBB32 -> processMul64(0, 0),
//      SMBT32 -> processMul64(0, 1),
//      SMTT32 -> processMul64(1, 1)
    )
  }
}
