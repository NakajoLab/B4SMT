package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._
import chisel3.util._

object PExt16AddSub {

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

  def pext16addsub = {
    import b4processor.modules.PExt.PExt16AddSub.DirectionType._
    import b4processor.modules.PExt.PExt16AddSub.AddSub._
    import b4processor.modules.PExt.PExt16AddSub.ProcessType._
    import PExtensionOperation._
    Seq[(PExtensionOperation.Type, (UInt, UInt) => (UInt, Bool))](
      // add
      ADD16 -> process16(Straight, Add, Add, Normal),
      RADD16 -> process16(Straight, Add, Add, SignedHalving),
      URADD16 -> process16(Straight, Add, Add, UnsignedHalving),
      KADD16 -> process16(Straight, Add, Add, SignedSaturate),
      UKADD16 -> process16(Straight, Add, Add, UnsignedSaturate),
      // sub
      SUB16 -> process16(Straight, Sub, Sub, Normal),
      RSUB16 -> process16(Straight, Sub, Sub, SignedHalving),
      URSUB16 -> process16(Straight, Sub, Sub, UnsignedHalving),
      KSUB16 -> process16(Straight, Sub, Sub, SignedSaturate),
      UKSUB16 -> process16(Straight, Sub, Sub, UnsignedSaturate),
      // cras
      CRAS16 -> process16(Cross, Add, Sub, Normal),
      RCRAS16 -> process16(Cross, Add, Sub, SignedHalving),
      URCRAS16 -> process16(Cross, Add, Sub, UnsignedHalving),
      KCRAS16 -> process16(Cross, Add, Sub, SignedSaturate),
      UKCRAS16 -> process16(Cross, Add, Sub, UnsignedSaturate),
      // crsa
      CRSA16 -> process16(Cross, Sub, Add, Normal),
      RCRSA16 -> process16(Cross, Sub, Add, SignedHalving),
      URCRSA16 -> process16(Cross, Sub, Add, UnsignedHalving),
      KCRSA16 -> process16(Cross, Sub, Add, SignedSaturate),
      UKCRSA16 -> process16(Cross, Sub, Add, UnsignedSaturate),
      // stas
      STAS16 -> process16(Straight, Add, Sub, Normal),
      RSTAS16 -> process16(Straight, Add, Sub, SignedHalving),
      URSTAS16 -> process16(Straight, Add, Sub, UnsignedHalving),
      KSTAS16 -> process16(Straight, Add, Sub, SignedSaturate),
      UKSTAS16 -> process16(Straight, Add, Sub, UnsignedSaturate),
      // stsa
      STSA16 -> process16(Straight, Sub, Add, Normal),
      RSTSA16 -> process16(Straight, Sub, Add, SignedHalving),
      URSTSA16 -> process16(Straight, Sub, Add, UnsignedHalving),
      KSTSA16 -> process16(Straight, Sub, Add, SignedSaturate),
      UKSTSA16 -> process16(Straight, Sub, Add, UnsignedSaturate),
    )
  }

  def process16(
    dir: DirectionType,
    high: AddSub,
    low: AddSub,
    ptype: ProcessType,
  ): (UInt, UInt) => (UInt, Bool) = (rs1: UInt, rs2: UInt) => {
    val out = Seq.fill(4)(Wire(UInt(16.W)))
    var overflow = WireInit(false.B)
    for (x <- Seq(1, 3)) {
      val ah17 = Wire(UInt(17.W))
      val bh17 = Wire(UInt(17.W))
      val al17 = Wire(UInt(17.W))
      val bl17 = Wire(UInt(17.W))

      ptype match {
        case ProcessType.Normal => {
          ah17 := rs1.H(x)
          bh17 := rs2.H(x)
          al17 := rs1.H(x - 1)
          bl17 := rs2.H(x - 1)
        }
        case ProcessType.SignedSaturate | ProcessType.SignedHalving => {
          ah17 := SE17(rs1.H(x))
          bh17 := SE17(rs2.H(x))
          al17 := SE17(rs1.H(x - 1))
          bl17 := SE17(rs2.H(x - 1))
        }
        case ProcessType.UnsignedSaturate | ProcessType.UnsignedHalving => {
          ah17 := ZE17(rs1.H(x))
          bh17 := ZE17(rs2.H(x))
          al17 := ZE17(rs1.H(x - 1))
          bl17 := ZE17(rs2.H(x - 1))
        }
      }

      val th17 = Wire(UInt(17.W))
      val tl17 = Wire(UInt(17.W))

      dir match {
        case DirectionType.Straight => {
          high match {
            case AddSub.Add => th17 := ah17 + bh17
            case AddSub.Sub => th17 := ah17 - bh17
          }

          low match {
            case AddSub.Add => tl17 := al17 + bl17
            case AddSub.Sub => tl17 := al17 - bl17
          }
        }
        case DirectionType.Cross => {
          high match {
            case AddSub.Add => th17 := ah17 + bl17
            case AddSub.Sub => th17 := ah17 - bl17
          }

          low match {
            case AddSub.Add => tl17 := al17 + bh17
            case AddSub.Sub => tl17 := al17 - bh17
          }
        }
      }

      ptype match {
        case ProcessType.Normal => {
          out(x) := th17
          out(x - 1) := tl17
        }
        case ProcessType.SignedHalving | ProcessType.UnsignedHalving => {
          out(x) := th17(16, 1)
          out(x - 1) := tl17(16, 1)
        }
        case ProcessType.SignedSaturate => {
          val out_th = SAT.Q15(th17)
          out(x) := out_th._1
          overflow = overflow | out_th._2
          val out_tl = SAT.Q15(tl17)
          out(x - 1) := out_tl._1
          overflow = overflow | out_tl._2
        }
        case ProcessType.UnsignedSaturate => {
          val out_th = SAT.U16(th17)
          out(x) := out_th._1
          overflow = overflow | out_th._2
          val out_tl = SAT.U16(tl17)
          out(x - 1) := out_tl._1
          overflow = overflow | out_tl._2
        }
      }
    }
    (out.reverse.reduce(_ ## _), overflow)
  }
}
