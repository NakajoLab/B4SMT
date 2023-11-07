package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._
import chisel3.util._

object PExtMisc2 {
  def pextMisc2(rs1: UInt, rs2: UInt, rs3: UInt, rd: UInt, imm: UInt) =
    Seq(
      AVE -> {
        val res = SE65(rs1) + SE65(rs2) + SE65(1.U)
        (res(64, 1), false.B)
      },
      SRA_U -> {
        val sa = rs2(5, 0)
        val res = Mux(
          sa =/= 0.U,
          (SE65((rs1.asSInt >> (sa - 1.U)).asUInt) + 1.U)(64, 1),
          rs1,
        )
        (res, false.B)
      },
      SRAI_U -> {
        val sa = imm(5, 0)
        val res = Mux(
          sa =/= 0.U,
          (SE65((rs1.asSInt >> (sa - 1.U)).asUInt) + 1.U)(64, 1),
          rs1,
        )
        (res, false.B)

      },
//      BITREV -> {
//        val msb = rs2(5, 0)
//        val rev = MuxCase(
//          0.U,
//          (0 until 64).map(i => {
//            (msb === i.U) -> {
//              Cat(rs1(i, 0).asBools)
//            }
//          })
//        )
//        (rev, false.B)
//      },
//      BITREVI -> {
//        val msb = imm(5, 0)
//        val rev = MuxCase(
//          0.U,
//          (0 until 64).map(i => {
//            (msb === i.U) -> {
//              Cat(rs1(i, 0).asBools)
//            }
//          })
//        )
//        (rev, false.B)
//      },
//      WEXT -> {
//        val LSBloc = rs2(4, 0)
//        val extractW = MuxCase(
//          0.U,
//          (0 until 31).map(i => {
//            (LSBloc === i.U) -> {
//              rs1(i + 31, i)
//            }
//          })
//        )
//        (SE64(extractW), false.B)
//      },
//      WEXTI -> {
//        {
//          val LSBloc = imm(4, 0)
//          val extractW = MuxCase(
//            0.U,
//            (0 until 31).map(i => {
//              (LSBloc === i.U) -> {
//                rs1(i + 31, i)
//              }
//            })
//          )
//          (SE64(extractW), false.B)
//        }
//      },
      CMIX -> {
        val res =
          Cat((0 until 64).map(x => Mux(rs2(x), rs1(x), rs3(x))).reverse)
        (res, false.B)
      },
      INSB -> {
        val bpos = imm(2, 0)
        val res =
          Cat(
            (0 until 8).reverse.map(x => Mux(bpos === x.U, rs1.B(0), rd.B(x))),
          )
        (res, false.B)
      },
      MADDR32 -> {
        val mresult = rs1.W(0) * rs2.W(0)
        val tres = rd.W(0) + mresult.W(0)
        (SE64(tres), false.B)
      },
      MAX -> (Mux(rs1.asSInt > rs2.asSInt, rs1, rs2), false.B),
      MIN -> (Mux(rs1.asSInt > rs2.asSInt, rs2, rs1), false.B),
    )
}
