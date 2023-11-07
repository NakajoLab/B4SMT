package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64_32AddSub {

  sealed trait DirectionType

  object DirectionType {
    case object Straight extends DirectionType
    case object Cross extends DirectionType
  }

  sealed trait AddSub

  object AddSub {
    case object Add extends AddSub

    case object Sub extends AddSub
  }

  sealed trait ProcessType
  object ProcessType {
    case object Normal extends ProcessType
    case object SignedHalving extends ProcessType
    case object UnsignedHalving extends ProcessType
    case object SignedSaturate extends ProcessType
    case object UnsignedSaturate extends ProcessType
  }

  def pext32addsub = {
    import PExt64_32AddSub.AddSub._
    import PExt64_32AddSub.DirectionType._
    import PExt64_32AddSub.ProcessType._
    import PExtensionOperation._
    Seq[(PExtensionOperation.Type, (UInt, UInt) => (UInt, Bool))](
      // add
      ADD32 -> process32(Straight, Add, Add, Normal),
      RADD32 -> process32(Straight, Add, Add, SignedHalving),
      URADD32 -> process32(Straight, Add, Add, UnsignedHalving),
      KADD32 -> process32(Straight, Add, Add, SignedSaturate),
      UKADD32 -> process32(Straight, Add, Add, UnsignedSaturate),
      // sub
      SUB32 -> process32(Straight, Sub, Sub, Normal),
      RSUB32 -> process32(Straight, Sub, Sub, SignedHalving),
      URSUB32 -> process32(Straight, Sub, Sub, UnsignedHalving),
      KSUB32 -> process32(Straight, Sub, Sub, SignedSaturate),
      UKSUB32 -> process32(Straight, Sub, Sub, UnsignedSaturate),
      // cras
      CRAS32 -> process32(Cross, Add, Sub, Normal),
      RCRAS32 -> process32(Cross, Add, Sub, SignedHalving),
      URCRAS32 -> process32(Cross, Add, Sub, UnsignedHalving),
      KCRAS32 -> process32(Cross, Add, Sub, SignedSaturate),
      UKCRAS32 -> process32(Cross, Add, Sub, UnsignedSaturate),
      // crsa
      CRSA32 -> process32(Cross, Sub, Add, Normal),
      RCRSA32 -> process32(Cross, Sub, Add, SignedHalving),
      URCRSA32 -> process32(Cross, Sub, Add, UnsignedHalving),
      KCRSA32 -> process32(Cross, Sub, Add, SignedSaturate),
      UKCRSA32 -> process32(Cross, Sub, Add, UnsignedSaturate),
      // stas
      STAS32 -> process32(Straight, Add, Sub, Normal),
      RSTAS32 -> process32(Straight, Add, Sub, SignedHalving),
      URSTAS32 -> process32(Straight, Add, Sub, UnsignedHalving),
      KSTAS32 -> process32(Straight, Add, Sub, SignedSaturate),
      UKSTAS32 -> process32(Straight, Add, Sub, UnsignedSaturate),
      // stsa
      STSA32 -> process32(Straight, Sub, Add, Normal),
      RSTSA32 -> process32(Straight, Sub, Add, SignedHalving),
      URSTSA32 -> process32(Straight, Sub, Add, UnsignedHalving),
      KSTSA32 -> process32(Straight, Sub, Add, SignedSaturate),
      UKSTSA32 -> process32(Straight, Sub, Add, UnsignedSaturate),
    )
  }

  def process32(
    dir: DirectionType,
    high: AddSub,
    low: AddSub,
    ptype: ProcessType,
  ): (UInt, UInt) => (UInt, Bool) = (rs1: UInt, rs2: UInt) => {
    val out = Seq.fill(2)(Wire(UInt(32.W)))
    var overflow = WireInit(false.B)
    for (x <- Seq(1)) {
      val ah33 = Wire(UInt(33.W))
      val bh33 = Wire(UInt(33.W))
      val al33 = Wire(UInt(33.W))
      val bl33 = Wire(UInt(33.W))

      ptype match {
        case ProcessType.Normal => {
          ah33 := rs1.W(x)
          bh33 := rs2.W(x)
          al33 := rs1.W(x - 1)
          bl33 := rs2.W(x - 1)
        }
        case ProcessType.SignedSaturate | ProcessType.SignedHalving => {
          ah33 := SE33(rs1.W(x))
          bh33 := SE33(rs2.W(x))
          al33 := SE33(rs1.W(x - 1))
          bl33 := SE33(rs2.W(x - 1))
        }
        case ProcessType.UnsignedSaturate | ProcessType.UnsignedHalving => {
          ah33 := ZE33(rs1.W(x))
          bh33 := ZE33(rs2.W(x))
          al33 := ZE33(rs1.W(x - 1))
          bl33 := ZE33(rs2.W(x - 1))
        }
      }

      val th33 = Wire(UInt(33.W))
      val tl33 = Wire(UInt(33.W))

      dir match {
        case DirectionType.Straight => {
          high match {
            case AddSub.Add => th33 := ah33 + bh33
            case AddSub.Sub => th33 := ah33 - bh33
          }

          low match {
            case AddSub.Add => tl33 := al33 + bl33
            case AddSub.Sub => tl33 := al33 - bl33
          }
        }
        case DirectionType.Cross => {
          high match {
            case AddSub.Add => th33 := ah33 + bl33
            case AddSub.Sub => th33 := ah33 - bl33
          }

          low match {
            case AddSub.Add => tl33 := al33 + bh33
            case AddSub.Sub => tl33 := al33 - bh33
          }
        }
      }

      ptype match {
        case ProcessType.Normal => {
          out(x) := th33
          out(x - 1) := tl33
        }
        case ProcessType.SignedHalving | ProcessType.UnsignedHalving => {
          out(x) := th33(32, 1)
          out(x - 1) := tl33(32, 1)
        }
        case ProcessType.SignedSaturate => {
          val out_th = SAT.Q31(th33)
          out(x) := out_th._1
          overflow = overflow | out_th._2
          val out_tl = SAT.Q31(tl33)
          out(x - 1) := out_tl._1
          overflow = overflow | out_tl._2
        }
        case ProcessType.UnsignedSaturate => {
          val out_th = SAT.U32(th33)
          out(x) := out_th._1
          overflow = overflow | out_th._2
          val out_tl = SAT.U32(tl33)
          out(x - 1) := out_tl._1
          overflow = overflow | out_tl._2
        }
      }
    }
    (out.reverse.reduce(_ ## _), overflow)
  }
}
