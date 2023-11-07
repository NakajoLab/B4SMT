package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt8AddSub {

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

  def pext8addsub = {
    import b4processor.modules.PExt.PExt8AddSub.AddSub._
    import b4processor.modules.PExt.PExt8AddSub.ProcessType._
    import b4processor.modules.PExt.PExtensionOperation._

    Seq[(PExtensionOperation.Type, (UInt, UInt) => (UInt, Bool))](
      // add
      ADD8 -> process8(Add, Normal),
      RADD8 -> process8(Add, SignedHalving),
      URADD8 -> process8(Add, UnsignedHalving),
      KADD8 -> process8(Add, SignedSaturate),
      UKADD8 -> process8(Add, UnsignedSaturate),
      // sub
      SUB8 -> process8(Sub, Normal),
      RSUB8 -> process8(Sub, SignedHalving),
      URSUB8 -> process8(Sub, UnsignedHalving),
      KSUB8 -> process8(Sub, SignedSaturate),
      UKSUB8 -> process8(Sub, UnsignedSaturate),
    )
  }

  def process8(op: AddSub, ptype: ProcessType) =
    (rs1: UInt, rs2: UInt) => {
      val out = Seq.fill(8)(Wire(UInt(8.W)))
      var overflow = false.B
      for (x <- 0 until 8) {
        val a9 = WireInit(0.U(9.W))
        val b9 = WireInit(0.U(9.W))

        ptype match {
          case ProcessType.Normal => {
            a9 := rs1.B(x)
            b9 := rs2.B(x)
          }
          case ProcessType.SignedSaturate | ProcessType.SignedHalving => {
            a9 := SE9(rs1.B(x))
            b9 := SE9(rs2.B(x))
          }
          case ProcessType.UnsignedSaturate | ProcessType.UnsignedHalving => {
            a9 := ZE9(rs1.B(x))
            b9 := ZE9(rs2.B(x))
          }
        }

        val t9 = WireInit(0.U(9.W))

        op match {
          case AddSub.Add => t9 := a9 + b9
          case AddSub.Sub => t9 := a9 - b9
        }

        ptype match {
          case ProcessType.Normal =>
            out(x) := t9

          case ProcessType.SignedHalving | ProcessType.UnsignedHalving =>
            out(x) := t9(8, 1)

          case ProcessType.SignedSaturate => {
            val out_t = SAT.Q7(t9)
            out(x) := out_t._1
            overflow = overflow | out_t._2
          }

          case ProcessType.UnsignedSaturate => {
            val out_t = SAT.U8(t9)
            out(x) := out_t._1
            overflow = overflow | out_t._2
          }
        }
      }
      (out.reverse.reduce(_ ## _), overflow)
    }
}
