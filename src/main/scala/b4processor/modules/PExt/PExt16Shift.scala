package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt16Shift {

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

  sealed trait ShiftProcess

  object ShiftProcess {
    case object RightArithmetic extends ShiftProcess
    case object RightArithmeticRound extends ShiftProcess

    case object RightLogical extends ShiftProcess
    case object RightLogicalRound extends ShiftProcess

    case object Left extends ShiftProcess
    case object LeftSaturating extends ShiftProcess

    case object LeftSaturatingRight extends ShiftProcess
    case object LeftSaturatingRightRound extends ShiftProcess
  }

  def pext16shift = {
    import PExtensionOperation._
    import b4processor.modules.PExt.PExt16Shift.ShiftProcess._
    Seq[(PExtensionOperation.Type, (UInt, UInt, UInt) => (UInt, Bool))](
      SRA16 -> processShift16(RightArithmetic, false),
      SRAI16 -> processShift16(RightArithmetic, true),
      SRA16_U -> processShift16(RightArithmeticRound, false),
      SRAI16_U -> processShift16(RightArithmeticRound, true),
      SRL16 -> processShift16(RightLogical, false),
      SRLI16 -> processShift16(RightLogical, true),
      SRL16_U -> processShift16(RightLogicalRound, false),
      SRLI16_U -> processShift16(RightLogicalRound, true),
      SLL16 -> processShift16(Left, false),
      SLLI16 -> processShift16(Left, true),
      KSLL16 -> processShift16(LeftSaturating, false),
      KSLLI16 -> processShift16(LeftSaturating, true),
      KSLRA16 -> processShift16(LeftSaturatingRight, false),
      KSLRA16_U -> processShift16(LeftSaturatingRightRound, false),
    )
  }

  def processShift16(ptype: ShiftProcess, useImm: Boolean) =
    (rs1: UInt, rs2: UInt, imm: UInt) => {
      val out = Seq.fill(4)(Wire(UInt(16.W)))
      var overflow = false.B
      for (x <- 0 until 4) {
        val amt =
          if (useImm)
            imm(3, 0)
          else
            rs2(3, 0)

        import ShiftProcess._
        val t = Wire(UInt(16.W))
        t := (ptype match {
          case RightArithmetic      => (rs1.H(x).asSInt >> amt).asUInt
          case RightArithmeticRound => RoundingShiftRightSigned16(rs1.H(x), amt)
          case RightLogical         => (rs1.H(x) >> amt).asUInt
          case RightLogicalRound => RoundingShiftRightUnsigned16(rs1.H(x), amt)
          case Left              => (rs1.H(x) << amt).asUInt
          case LeftSaturating => {
            val out_t = SaturatingShiftLeft16(rs1.H(x), amt)
            overflow = overflow | out_t._2
            out_t._1
          }
          case LeftSaturatingRight => {
            val amts = rs2.H(x)(4, 0).asSInt
            val a = rs1.H(x)
            val out_t = SAT.Q15((a << amts.asUInt).asUInt)
            overflow = (overflow | Mux(amts < 0.S, false.B, out_t._2))
            Mux(amts < 0.S, (a.asSInt >> (0.S - amts).asUInt).asUInt, out_t._1)
          }
          case LeftSaturatingRightRound => {
            val amts = rs2.H(x)(4, 0).asSInt
            val a = rs1.H(x)
            val out_t = SAT.Q15((a << amts.asUInt).asUInt)
            overflow = (overflow | Mux(amts < 0.S, false.B, out_t._2))
            Mux(
              amts < 0.S,
              RoundingShiftRightSigned16(a, (0.S - amts).asUInt),
              out_t._1,
            )
          }
        })
        out(x) := t
      }
      (out.reverse.reduce(_ ## _), overflow)
    }
}
