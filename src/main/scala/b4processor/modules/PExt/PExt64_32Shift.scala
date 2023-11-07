package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt64_32Shift {

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

  def pext32shift = {
    import PExt64_32Shift.ShiftProcess._
    import PExtensionOperation._
    Seq[(PExtensionOperation.Type, (UInt, UInt, UInt) => (UInt, Bool))](
      SRA32 -> processShift32(RightArithmetic, false),
      SRAI32 -> processShift32(RightArithmetic, true),
      SRA32_U -> processShift32(RightArithmeticRound, false),
      SRAI32_U -> processShift32(RightArithmeticRound, true),
      SRL32 -> processShift32(RightLogical, false),
      SRLI32 -> processShift32(RightLogical, true),
      SRL32_U -> processShift32(RightLogicalRound, false),
      SRLI32_U -> processShift32(RightLogicalRound, true),
      SLL32 -> processShift32(Left, false),
      SLLI32 -> processShift32(Left, true),
      KSLL32 -> processShift32(LeftSaturating, false),
      KSLLI32 -> processShift32(LeftSaturating, true),
      KSLRA32 -> processShift32(LeftSaturatingRight, false),
      KSLRA32_U -> processShift32(LeftSaturatingRightRound, false),
    )
  }

  def processShift32(ptype: ShiftProcess, useImm: Boolean) =
    (rs1: UInt, rs2: UInt, imm: UInt) => {
      val out = Seq.fill(2)(Wire(UInt(32.W)))
      var overflow = false.B
      for (x <- 0 until 2) {
        val amt =
          if (useImm)
            imm(4, 0)
          else
            rs2(4, 0)

        import ShiftProcess._
        val t = Wire(UInt(32.W))
        t := (ptype match {
          case RightArithmetic      => (rs1.W(x).asSInt >> amt).asUInt
          case RightArithmeticRound => RoundingShiftRightSigned32(rs1.W(x), amt)
          case RightLogical         => (rs1.W(x) >> amt).asUInt
          case RightLogicalRound => RoundingShiftRightUnsigned32(rs1.W(x), amt)
          case Left              => (rs1.W(x) << amt).asUInt
          case LeftSaturating => {
            val out_t = SaturatingShiftLeft32(rs1.W(x), amt)
            overflow = overflow | out_t._2
            out_t._1
          }
          case LeftSaturatingRight => {
            val amts = rs2.W(x)(4, 0).asSInt
            val a = rs1.W(x)
            val out_t = SAT.Q31((a << amts.asUInt).asUInt)
            overflow = (overflow | Mux(amts < 0.S, false.B, out_t._2))
            Mux(amts < 0.S, (a.asSInt >> (0.S - amts).asUInt).asUInt, out_t._1)
          }
          case LeftSaturatingRightRound => {
            val amts = rs2.W(x)(4, 0).asSInt
            val a = rs1.W(x)
            val out_t = SAT.Q31((a << amts.asUInt).asUInt)
            overflow = (overflow | Mux(amts < 0.S, false.B, out_t._2))
            Mux(
              amts < 0.S,
              RoundingShiftRightSigned32(a, (0.S - amts).asUInt),
              out_t._1,
            )
          }
        })
        out(x) := t
      }
      (out.reverse.reduce(_ ## _), overflow)
    }
}
