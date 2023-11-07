package b4processor.modules.PExt

import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt8Shift {
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

  def pext8shift = {
    import PExtensionOperation._
    import b4processor.modules.PExt.PExt8Shift.ShiftProcess._
    Seq[(PExtensionOperation.Type, (UInt, UInt, UInt) => (UInt, Bool))](
      SRA8 -> processShift8(RightArithmetic, false),
      SRAI8 -> processShift8(RightArithmetic, true),
      SRA8_U -> processShift8(RightArithmeticRound, false),
      SRAI8_U -> processShift8(RightArithmeticRound, true),
      SRL8 -> processShift8(RightLogical, false),
      SRLI8 -> processShift8(RightLogical, true),
      SRL8_U -> processShift8(RightLogicalRound, false),
      SRLI8_U -> processShift8(RightLogicalRound, true),
      SLL8 -> processShift8(Left, false),
      SLLI8 -> processShift8(Left, true),
      KSLL8 -> processShift8(LeftSaturating, false),
      KSLLI8 -> processShift8(LeftSaturating, true),
      KSLRA8 -> processShift8(LeftSaturatingRight, false),
      KSLRA8_U -> processShift8(LeftSaturatingRightRound, false),
    )
  }

  def processShift8(ptype: ShiftProcess, useImm: Boolean) =
    (rs1: UInt, rs2: UInt, imm: UInt) => {
      val out = Seq.fill(8)(Wire(UInt(8.W)))
      var overflow = false.B
      for (x <- 0 until 8) {
        val amt =
          if (useImm)
            imm(2, 0)
          else
            rs2(2, 0)

        import ShiftProcess._
        val t = Wire(UInt(8.W))
        t := (ptype match {
          case RightArithmetic      => (rs1.B(x).asSInt >> amt).asUInt
          case RightArithmeticRound => RoundingShiftRightSigned8(rs1.B(x), amt)
          case RightLogical         => (rs1.B(x) >> amt).asUInt
          case RightLogicalRound => RoundingShiftRightUnsigned8(rs1.B(x), amt)
          case Left              => (rs1.B(x) << amt).asUInt
          case LeftSaturating => {
            val out_t = SaturatingShiftLeft8(rs1.B(x), amt)
            overflow = overflow | out_t._2
            out_t._1
          }
          case LeftSaturatingRight => {
            val amts = rs2.B(x)(3, 0).asSInt
            val a = rs1.B(x)
            val out_t = SAT.Q15((a << amts.asUInt).asUInt)
            overflow = (overflow | Mux(amts < 0.S, false.B, out_t._2))
            Mux(amts < 0.S, (a.asSInt >> (0.S - amts).asUInt).asUInt, out_t._1)
          }
          case LeftSaturatingRightRound => {
            val amts = rs2.B(x)(4, 0).asSInt
            val a = rs1.B(x)
            val out_t = SAT.Q7((a << amts.asUInt).asUInt)
            overflow = (overflow | Mux(amts < 0.S, false.B, out_t._2))
            Mux(
              amts < 0.S,
              RoundingShiftRightSigned8(a, (0.S - amts).asUInt),
              out_t._1,
            )
          }
        })
        out(x) := t
      }
      (out.reverse.reduce(_ ## _), overflow)
    }
}
