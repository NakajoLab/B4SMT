package b4processor.modules.PExt

import _root_.circt.stage.ChiselStage
import b4processor.modules.PExt.PExt16AddSub.pext16addsub
import b4processor.modules.PExt.PExt16Compare.pext16cmp
import b4processor.modules.PExt.PExt16Misc.pext16misc
import b4processor.modules.PExt.PExt16Multiply.pext16mul
import b4processor.modules.PExt.PExt16Pack.pext16pack
import b4processor.modules.PExt.PExt16Shift.{pext16shift, processShift16}
import b4processor.modules.PExt.PExt32Computation.pext32Computation
import b4processor.modules.PExt.PExt64_32AddSub.pext32addsub
import b4processor.modules.PExt.PExt64DataComputation.pext64DataComputation
import b4processor.modules.PExt.PExt64_32Misc.pext32misc
import b4processor.modules.PExt.PExt64_32Multiply.pext32mul
import b4processor.modules.PExt.PExt64_32MultiplyAndAdd.pext64_32MultiplyAndAdd
import b4processor.modules.PExt.PExt64_32ParallelMultiplyAndAdd.pext64_32ParallelMultiplyAndAdd
import b4processor.modules.PExt.PExt64_32Shift.pext32shift
import b4processor.modules.PExt.PExt64_32packing.pext64_32packing
import b4processor.modules.PExt.PExt64_NonSIMD32Shift.pext64_NonSIMD32Shift
import b4processor.modules.PExt.PExt64_Q15.pextQ15
import b4processor.modules.PExt.PExt8AddSub.pext8addsub
import b4processor.modules.PExt.PExt8Compare.pext8cmp
import b4processor.modules.PExt.PExt8Misc.pext8misc
import b4processor.modules.PExt.PExt8MulWith32Add.pext8MulWith32Add
import b4processor.modules.PExt.PExt8Multiply.pext8mul
import b4processor.modules.PExt.PExt8Shift.pext8shift
import b4processor.modules.PExt.PExt8Unpack.pext8unpack
import b4processor.modules.PExt.PExtMSW32x16MulAdd.pextMsw32x16
import b4processor.modules.PExt.PExtMSW32x32MulAdd.pextMsw32x32
import b4processor.modules.PExt.PExtMisc.pextMisc
import b4processor.modules.PExt.PExtMisc2.pextMisc2
import b4processor.modules.PExt.PExtQ16Saturate.pextQ16Saturate
import b4processor.modules.PExt.PExtQ32Saturate.pextQ32Saturate
import b4processor.modules.PExt.PExtSigned16MulWith32AddSub.pextSigned16MulWith32AddSub
import chisel3._
import chisel3.util._

import scala.math.pow

object PExtensionOperation extends ChiselEnum {
  // 16 add sub
  val ADD16, RADD16, URADD16, KADD16, UKADD16, SUB16, RSUB16, URSUB16, KSUB16,
    UKSUB16, CRAS16, RCRAS16, URCRAS16, KCRAS16, UKCRAS16, CRSA16, RCRSA16,
    URCRSA16, KCRSA16, UKCRSA16, STAS16, RSTAS16, URSTAS16, KSTAS16, UKSTAS16,
    STSA16, RSTSA16, URSTSA16, KSTSA16, UKSTSA16 = Value
  // 8 add sub
  val ADD8, RADD8, URADD8, KADD8, UKADD8, SUB8, RSUB8, URSUB8, KSUB8, UKSUB8 =
    Value
  // 16 shift
  val SRA16, SRAI16, SRA16_U, SRAI16_U, SRL16, SRLI16, SRL16_U, SRLI16_U, SLL16,
    SLLI16, KSLL16, KSLLI16, KSLRA16, KSLRA16_U = Value
  // 8 shift
  val SRA8, SRAI8, SRA8_U, SRAI8_U, SRL8, SRLI8, SRL8_U, SRLI8_U, SLL8, SLLI8,
    KSLL8, KSLLI8, KSLRA8, KSLRA8_U = Value
  // 16 cmp
  val CMPEQ16, SCMPLT16, SCMPLE16, UCMPLT16, UCMPLE16 = Value
  // 8 cmp
  val CMPEQ8, SCMPLT8, SCMPLE8, UCMPLT8, UCMPLE8 = Value
  // 16 mul
  val SMUL16, SMULX16, UMUL16, UMULX16, KHM16, KHMX16 = Value
  // 8 mul
  val SMUL8, SMULX8, UMUL8, UMULX8, KHM8, KHMX8 = Value
  // 16 misc
  val SMIN16, UMIN16, SMAX16, UMAX16, SCLIP16, UCLIP16, KABS16, CLRS16, CLZ16 =
    Value // SWAP16 <- alias
  // 8 misc
  val SMIN8, UMIN8, SMAX8, UMAX8, SCLIP8, UCLIP8, KABS8, CLRS8, CLZ8 =
    Value // SWAP8 <- alias
  // 8 unpack
  val SUNPKD810, SUNPKD820, SUNPKD830, SUNPKD831, SUNPKD832, ZUNPKD810,
    ZUNPKD820, ZUNPKD830, ZUNPKD831, ZUNPKD832 = Value
  // 16 pack
  val PKBB16, PKBT16, PKTB16, PKTT16 = Value
  // MSW 32x32 mul add
  val SMMUL, SMMUL_U, KMMAC, KMMAC_U, KMMSB, KMMSB_U, KWMMUL, KWMMUL_U = Value
  // MSW 32x16 mul add
  val SMMWB, SMMWB_U, SMMWT, SMMWT_U, KMMAWB, KMMAWB_U, KMMAWT, KMMAWT_U,
    KMMWB2, KMMWB2_U, KMMWT2, KMMWT2_U, KMMAWB2, KMMAWB2_U, KMMAWT2, KMMAWT2_U =
    Value
  // signed 16 mul with 32 add sub
  val SMBB16, SMBT16, SMTT16, KMDA, KMXDA, SMDS, SMDRS, SMXDS, KMABB, KMABT,
    KMATT, KMADA, KMAXDA, KMADS, KMADRS, KMAXDS, KMSDA, KMSXDA = Value
  // signed 16 mul with 64 add sub
  val SMAL = Value
  // MISC
  val SCLIP32, UCLIP32, CLRS32, CLZ32, PBSAD, PBSADA = Value
  // 8bul with 32 add
  val SMAQA, UMAQA, SMAQASU = Value
  // 64 data computation
  val /*ADD64,TODO */ RADD64, URADD64, KADD64, UKADD64, /*SUB64, TODO*/ RSUB64,
    URSUB64, KSUB64, UKSUB64 = Value
  // 32 mul with 64 add sub
  val SMAR64, SMSR64, UMAR64, UMSR64, KMAR64, KMSR64, UKMAR64, UKMSR64 = Value
  // signed 16 mul with 64 add/sub
  val SMALBB, SMALBT, SMALTT, SMALDA, SMALXDA, SMALDS, SMALDRS, SMALXDS, SMSLDA,
    SMSLXDA = Value
  // non simd Q15 saturate
  val KADDH, KSUBH, KHMBB, KHMBT, KHMTT, UKADDH, UKSUBH = Value
  // Q31 saturate
  val KADDW, UKADDW, KSUBW, UKSUBW, KDMBB, KDMBT, KDMTT, KSLRAW, KSLRAW_U,
    KSLLW, KSLLIW, KDMABB, KDMABT, KDMATT, KABSW = Value
  // 32 computation
  val RADDW, URADDW, RSUBW, URSUBW, MULR64, MULSR64, MSUBR32 = Value
  // overflow saturation status manipulation (pseudo)
//    RDOV,CLROV,
  // Misc2
  val AVE, SRA_U, SRAI_U,
  /*BITREV,  BITREVI, TODO:? */ /*WEXT, WEXTI, TODO:OK? */ CMIX, INSB,
    MADDR32, // MSUBR32,???
  MAX, MIN = Value
  // RV64 only add sub
  val ADD32, RADD32, URADD32, KADD32, UKADD32, SUB32, RSUB32, URSUB32, KSUB32,
    UKSUB32, CRAS32, RCRAS32, URCRAS32, KCRAS32, UKCRAS32, CRSA32, RCRSA32,
    URCRSA32, KCRSA32, UKCRSA32, STAS32, RSTAS32, URSTAS32, KSTAS32, UKSTAS32,
    STSA32, RSTSA32, URSTSA32, KSTSA32, UKSTSA32 = Value
  // 64 only 32 shift
  val SRA32, SRAI32, SRA32_U, SRAI32_U, SRL32, SRLI32, SRL32_U, SRLI32_U, SLL32,
    SLLI32, KSLL32, KSLLI32, KSLRA32, KSLRA32_U = Value
  // 64 only 32 misc
  val SMIN32, UMIN32, SMAX32, UMAX32, KABS32 = Value
  // 64 only Q15
  val KHMBB16, KHMBT16, KHMTT16, KDMBB16, KDMBT16, KDMTT16, KDMABB16, KDMABT16,
    KDMATT16 = Value
  // 64 only 32 mul TODO: is this correct? does not seem like an alias
  /* val SMBB32, SMBT32, SMTT32 = Value */
  // 64 only 32 mul and add
  val KMABB32, KMABT32, KMATT32 = Value
  // 64 only 32 parallel mul and add
  val KMDA32, KMXDA32, /*KMADA32, alias*/ KMAXDA32, KMADS32, KMADRS32, KMAXDS32,
    KMSDA32, KMSXDA32, SMDS32, SMDRS32, SMXDS32 = Value
  // 64 only non simd 32 shift
  val SRAIW_U = Value
//  // 64 only 32 pack
//  val PKBB32, PKBT32, PKTB32, PKTT32 = Value
}

class PExtExecutor extends Module {
  val io = IO(new Bundle {
    val input = Input(new Bundle {
      val oeration = new PExtensionOperation.Type()
      val rs1 = UInt(64.W)
      val rs2 = UInt(64.W)
      val rs3 = UInt(64.W)
      val rd = UInt(64.W)
      val imm = UInt(6.W)
    })
    val output = Output(new Bundle {
      val value = UInt(64.W)
      val overflow = Bool()
    })
  })

  val instructions: Seq[(PExtensionOperation.Type, (UInt, Bool))] =
    pext16addsub.map { case (a, b) => a -> b(io.input.rs1, io.input.rs2) } ++
      pext8addsub.map { case (a, b) => a -> b(io.input.rs1, io.input.rs2) } ++
      pext16shift.map { case (a, b) =>
        a -> b(io.input.rs1, io.input.rs2, io.input.imm)
      } ++
      pext8shift.map(a =>
        a._1 -> a._2(io.input.rs1, io.input.rs2, io.input.imm),
      ) ++
      pext16cmp.map(a => a._1 -> a._2(io.input.rs1, io.input.rs2)) ++
      pext8cmp.map(a => a._1 -> a._2(io.input.rs1, io.input.rs2)) ++
      pext16mul.map(a => a._1 -> a._2(io.input.rs1, io.input.rs2)) ++
      pext8mul.map(a => a._1 -> a._2(io.input.rs1, io.input.rs2)) ++
      pext16misc(io.input.rs1, io.input.rs2, io.input.imm) ++
      pext8misc(io.input.rs1, io.input.rs2, io.input.imm) ++
      pext8unpack(io.input.rs1) ++
      pext16pack(io.input.rs1, io.input.rs2) ++
      pextMsw32x32(io.input.rs1, io.input.rs2, io.input.rd) ++
      pextMsw32x16(io.input.rs1, io.input.rs2, io.input.rd) ++
      pextSigned16MulWith32AddSub(io.input.rs1, io.input.rs2, io.input.rd) ++
//    pextSigned16MulWith64AddSub(io.input.rs1,io.input.rs2) ++
      pextMisc(io.input.rs1, io.input.rs2, io.input.rd, io.input.imm) ++
      pext8MulWith32Add(
        io.input.rs1,
        io.input.rs2,
        io.input.rd,
        io.input.imm,
      ) ++
      pext64DataComputation(io.input.rs1, io.input.rs2) ++
      pextMsw32x16(io.input.rs1, io.input.rs2, io.input.rd) ++
      pextMsw32x32(io.input.rs1, io.input.rs2, io.input.rd) ++
      pextQ16Saturate(io.input.rs1, io.input.rs2, io.input.rd) ++
      pextQ32Saturate(io.input.rs1, io.input.rs2, io.input.rd, io.input.imm) ++
      pext32Computation(
        io.input.rs1,
        io.input.rs2,
        io.input.rd,
        io.input.imm,
      ) ++
      pextMisc2(
        io.input.rs1,
        io.input.rs2,
        io.input.rs3,
        io.input.rd,
        io.input.imm,
      ) ++
      pext32addsub.map { case (a, b) => a -> b(io.input.rs1, io.input.rs2) } ++
      pext32shift.map(a =>
        a._1 -> a._2(io.input.rs1, io.input.rs2, io.input.imm),
      ) ++
      pext32misc(io.input.rs1, io.input.rs2, io.input.imm) ++
      pextQ15(io.input.rs1, io.input.rs2, io.input.rd) ++
      pext32mul(io.input.rs1, io.input.rs2) ++
      pext64_32MultiplyAndAdd(io.input.rs1, io.input.rs2, io.input.rd) ++
      pext64_32ParallelMultiplyAndAdd(
        io.input.rs1,
        io.input.rs2,
        io.input.rd,
      ) ++
      pext64_NonSIMD32Shift(io.input.rs1, io.input.imm) ++
      pext64_32packing(io.input.rs1, io.input.rs2)

  io.output.value := MuxLookup(io.input.oeration, 0.U)(instructions.map {
    case (a, b) => a -> b._1
  })

  io.output.overflow := MuxLookup(io.input.oeration, 0.U)(instructions.map {
    case (a, b) => a -> b._2
  })
}

object UIntSectionHelper {
  implicit class UIntSection(u: UInt) {
    def W(x: Int) = u(x * 32 + 31, x * 32)
    def H(x: Int) = u(x * 16 + 15, x * 16)
    def B(x: Int) = u(x * 8 + 7, x * 8)
  }

  def SE(n: Int)(x: UInt) = {
    require(x.getWidth <= n)
    val t = Wire(SInt(n.W))
    t := x.asSInt
    t.asUInt
  }

  def SE17(x: UInt) = SE(17)(x)
  def SE64(x: UInt) = SE(64)(x)
  def SE65(x: UInt) = SE(65)(x)
  def SE33(x: UInt) = SE(33)(x)

  def SE16(x: UInt) = SE(16)(x)

  def SE9(x: UInt) = SE(9)(x)

  def ZE(n: Int)(x: UInt) = {
    require(x.getWidth <= n)
    val t = Wire(UInt(n.W))
    t := x
    t
  }

  def ZE17(x: UInt) = ZE(17)(x)
  def ZE33(x: UInt) = ZE(33)(x)
  def ZE16(x: UInt) = ZE(16)(x)

  def ZE9(x: UInt) = ZE(9)(x)

  object SAT {

    def Q(n: Int) = (x: UInt) => {
      val v = x.asSInt
      val max = (pow(2, n) - 1).toInt.S
      val min = -pow(2, n).toInt.S
      (
        Mux(v > max, max, Mux(v < min, min, v)).asUInt(n, 0),
        Mux(v > max, true.B, Mux(v < min, true.B, false.B)),
      )
    }

    def Q31 = Q(31)
    def Q63 = Q(63)

    def Q15 = Q(15)

    def Q7 = Q(7)

    def Q(n: UInt) = (x: UInt) => {
      val v = x.asSInt
      val amt = n

      (
        MuxLookup(amt, 0.U)(
          (0 until pow(2, n.getWidth).toInt).map(i =>
            i.U -> {
              val max = (pow(2, i) - 1).toInt.S
              val min = -pow(2, i).toInt.S
              Mux(v > max, max, Mux(v < min, min, v)).asUInt
            },
          ),
        ),
        MuxLookup(amt, false.B)(
          (0 until pow(2, n.getWidth).toInt).map(i =>
            i.U -> {
              val max = (pow(2, i) - 1).toInt.S
              val min = -pow(2, i).toInt.S
              Mux(v > max, true.B, Mux(v < min, true.B, false.B))
            },
          ),
        ),
      )
    }

    def U(n: UInt) = (x: UInt) => {
      val amt = n
      (
        MuxLookup(amt, 0.U)(
          (0 until pow(2, n.getWidth).toInt).map(i =>
            i.U -> {
              val max = (pow(2, i) - 1).toInt.U
              Mux(x > max, max, x)
            },
          ),
        ),
        MuxLookup(amt, false.B)(
          (0 until pow(2, n.getWidth).toInt).map(i =>
            i.U -> {
              val max = (pow(2, i) - 1).toInt.U
              Mux(x > max, true.B, false.B)
            },
          ),
        ),
      )
    }

    def U(n: Int) = (x: UInt) => {
      val max = (pow(2, n) - 1).toInt.U
      (Mux(x > max, max, x), Mux(x > max, true.B, false.B))
    }
    def U64 = U(64)

    def U32 = U(32)

    def U16 = U(16)

    def U8 = U(8)

  }

  def ABS(x: UInt) = {
    val v = x.asSInt
    v.abs.asUInt
  }

  def RoundingShiftRightUnsigned16(x: UInt, shamt: UInt) = {
    val amt = shamt(3, 0)
    Mux(amt === 0.U, x, ZE17((x >> (amt - 1.U)).asUInt + 1.U)(16, 1))
  }
  def RoundingShiftRightSigned16(x: UInt, shamt: UInt) = {
    val amt = shamt(3, 0)
    Mux(
      amt === 0.U,
      x,
      SE17(((x.asSInt >> (amt - 1.U)).asSInt + 1.S).asUInt)(16, 1),
    )
  }

  def RoundingShiftRightUnsigned32(x: UInt, shamt: UInt) = {
    val amt = shamt(4, 0)
    Mux(amt === 0.U, x, ZE33((x >> (amt - 1.U)).asUInt + 1.U)(32, 1))
  }

  def RoundingShiftRightSigned32(x: UInt, shamt: UInt) = {
    val amt = shamt(4, 0)
    Mux(
      amt === 0.U,
      x,
      SE33(((x.asSInt >> (amt - 1.U)).asSInt + 1.S).asUInt)(32, 1),
    )
  }

  def SaturatingShiftLeft16(x: UInt, shamt: UInt) = {
    val amt = shamt(3, 0)
    val shifted = (x << amt).asUInt
    SAT.Q15(shifted)
  }

  def SaturatingShiftLeft32(x: UInt, shamt: UInt) = {
    val amt = shamt(4, 0)
    val shifted = (x << amt).asUInt
    SAT.Q31(shifted)
  }

  def RoundingShiftRightUnsigned8(x: UInt, shamt: UInt) = {
    val amt = shamt(2, 0)
    Mux(amt === 0.U, x, ZE9((x >> (amt - 1.U)).asUInt + 1.U)(8, 1))
  }

  def RoundingShiftRightSigned8(x: UInt, shamt: UInt) = {
    val amt = shamt(2, 0)
    Mux(
      amt === 0.U,
      x,
      SE9(((x.asSInt >> (amt - 1.U)).asSInt + 1.S).asUInt)(8, 1),
    )
  }

  def SaturatingShiftLeft8(x: UInt, shamt: UInt) = {
    val amt = shamt(2, 0)
    val shifted = (x << amt).asUInt
    SAT.Q7(shifted)
  }

  def ROUND(x: UInt, lsb: UInt) = x + lsb

  def CLRS(x: UInt) = {
    MuxCase(
      0.U,
      (0 until x.getWidth - 1).reverse.map(i => {
        (x(x.getWidth - 1) === x(i)) -> (x.getWidth - i).U
      }),
    )
  }

  def CLZ(x: UInt) = {
    MuxCase(
      0.U,
      (0 until x.getWidth).reverse.map(i => {
        (x(i) === 0.U) -> (x.getWidth - i).U
      }),
    )
  }
}

object PExtExecutor extends App {
  ChiselStage.emitSystemVerilogFile(
    new PExtExecutor,
    firtoolOpts = Array(
      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb",
    ),
  )

}
