package b4processor.modules.PExt

import _root_.circt.stage.ChiselStage
import b4processor.modules.PExt.PExt16AddSub.pext16addsub
import b4processor.modules.PExt.PExt16Compare.pext16cmp
import b4processor.modules.PExt.PExt16Misc.pext16misc
import b4processor.modules.PExt.PExt16Multiply.pext16mul
import b4processor.modules.PExt.PExt16Pack.pext16pack
import b4processor.modules.PExt.PExt16Shift.{pext16shift, processShift16}
import b4processor.modules.PExt.PExt64DataComputation.pext64DataComputation
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
import b4processor.modules.PExt.PExtSigned16MulWith32AddSub.pextSigned16MulWith32AddSub
import b4processor.modules.PExt.PExtSigned16MulWith64AddSub.pextSigned16MulWith64AddSub
import chisel3._
import chisel3.util._

import scala.math.pow

object PExtensionOperation extends ChiselEnum {
  val None,
  // 16 add sub
  Add16, RAdd16, URAdd16, KAdd16, UKAdd16, Sub16, RSub16, URSub16, KSub16,
    UKSub16, CRAS16, RCRAS16, URCRAS16, KCRAS16, UKCRAS16, CRSA16, RCRSA16,
    URCRSA16, KCRSA16, UKCRSA16, STAS16, RSTAS16, URSTAS16, KSTAS16, UKSTAS16,
    STSA16, RSTSA16, URSTSA16, KSTSA16, UKSTSA16,
  // 8 add sub
  Add8, RAdd8, URAdd8, KAdd8, UKAdd8, Sub8, RSub8, URSub8, KSub8, UKSub8,
  // 16 shift
  SRA16, SRAI16, SRA16u, SRAI16u, SRL16, SRLI16, SRL16u, SRLI16u, SLL16, SLLI16,
    KSLL16, KSLLI16, KSLRA16, KSLRA16u,
  // 8 shift
  SRA8, SRAI8, SRA8u, SRAI8u, SRL8, SRLI8, SRL8u, SRLI8u, SLL8, SLLI8, KSLL8,
    KSLLI8, KSLRA8, KSLRA8u,
  // 16 cmp
  CMPEQ16, SCMPLT16, SCMPLE16, UCMPLT16, UCMPLE16,
  // 8 cmp
  CMPEQ8, SCMPLT8, SCMPLE8, UCMPLT8, UCMPLE8,
  // 16 mul
  SMUL16, SMULX16, UMUL16, UMULX16, KHM16, KHMX16,
  // 8 mul
  SMUL8, SMULX8, UMUL8, UMULX8, KHM8, KHMX8,
  // 16 misc
  SMIN16,UMIN16,SMAX16,UMAX16,SCLIP16,UCLIP16,KABS16,CLRS16,CLZ16,SWAP16,
  // 8 misc
  SMIN8, UMIN8, SMAX8, UMAX8, SCLIP8, UCLIP8, KABS8, CLRS8, CLZ8, SWAP8,
  // 8 unpack
    SUNPKD810,SUNPKD820,SUNPKD830,SUNPKD831,SUNPKD832,
  ZUNPKD810,ZUNPKD820,ZUNPKD830,ZUNPKD831,ZUNPKD832,
  // 16 pack
    PKBB16,PKBT16,PKTB16,PKTT16,
    // MSW 32x32 mul add
    SMMUL,SMMULu,KMMAC,KMMACu,KMMSB,KMMSBu,KWMMUL,KWMMULu,
    // MSW 32x16 mul add
    SMMWB, SMMWBu, SMMWT,SMMWTu, KMMAWB, KMMAWBu, KMMAWT, KMMAWTu,KMMWB2,KMMWB2u,KMMWT2,KMMWT2u,KMMAWB2,KMMAWB2u,KMMAWT2,KMMAWT2u,
  // signed 16 mul with 32 add sub
  SMBB16,SMBT16,SMTT16,KMDA,KMXDA,SMDS,SMDRS,SMXDS,KMABB,KMABT,KMATT,KMADA,KMAXDA,KMADS,KMADRS,KMAXDS,KMSDA,KMSXDA,
  // signed 16 mul with 64 add sub
    SMAL,
    // MISC
    SCLIP32,UCLIP32,CLRS32,CLZ32,PBSAD,PBSADA,
  //8bul with 32 add
    SMAQA,UMAQA,SMAQASU,
    // 64 data computation
    ADD64,RADD64,URADD64,KADD64,UKADD64,SUB64,RSUB64,URSUB64,KSUB64,UKSUB64,
    // 32 mul with 64 add sub
    SMAR64,SMSR64,UMAR64,UMSR64,KMAR64,KMSR64,UKMAR64,UKMSR64,
  // signed 16 mul with 64 add/sub
    SMALBB,SMALBT,SMALTT,SMALDA,SMALXDA,SMALDS,SMALDRS,SMALXDS,SMSLDA,SMSLXDA,
  // non simd
    KADDH,KSUBH, KHMBB,KHMBT,KHMTT,UKADDH,UKSUBH,
  // Q31 saturate
    KADDW,UKADDW,KSUBW,UKSUBW,KDMBB,KDMBT,KDMTT,KSLRAW,KSLRAWu,KSLLIW,KDMABB,KDMABT,KDMATT,KABSW,
  // 32 computation
    RADDW,URADDW,RSUBW,URSUBW,MULR64,MULSR64,MSUBR32,
  // overflow saturation status manipulation
    RDOV,CLROV,
  // Misc2
    AVE,SRAu,SRAIu,BITREV,BITREVI,WEXT,WEXTI,CMIX,INSB,MADDR32,//MSUBR32,???
    MAX,MIN,
  // RV64 only
  Add32, RAdd32, URAdd32, KAdd32, UKAdd32, Sub32, RSub32, URSub32, KSub32,
  UKSub32, CRAS32, RCRAS32, URCRAS32, KCRAS32, UKCRAS32, CRSA32, RCRSA32,
  URCRSA32, KCRSA32, UKCRSA32, STAS32, RSTAS32, URSTAS32, KSTAS32, UKSTAS32,
  STSA32, RSTSA32, URSTSA32, KSTSA32, UKSTSA32,
  // 64 only 32 shift
  SRA32, SRAI32, SRA32u, SRAI32u, SRL32, SRLI32, SRL32u, SRLI32u, SLL32, SLLI32,
  KSLL32, KSLLI32, KSLRA32, KSLRA32u,
  // 64 only 32 misc
    SMIN32,UMIN32,SMAX32,UMAX32,KABS32,
  // 64 only Q15
    KHMBB16,KHMBT16,KHMTT16,KDMBB16,KDMBT16,KDMTT16,KDMABT16,KDMATT16,
  // 64 only 32 mul
    SMBB32,SMBT32,SMTT32,
  // 64 only 32 mul and add
  KMABB32,KMABT32,KMATT32,
  // 64 only 32 parallel mul and add
    KMDA32,KMXDA32,KMADA32,KMAXDA32,KMAD32,KMADR32,KMAXDS32,KMSDA32,KMSXDA32,SMDS32,SMDRS32,SMXDS32,
  // 64 only non simd 32 shift
    SRAIWu,
    // 64 only 32 pack
    PKBB32,PKBT32,PKTB32,PKTT32
  = Value
}

class PExtExecutor extends Module {
  val io = IO(new Bundle {
    val input = Input(new Bundle {
      val oeration = new PExtensionOperation.Type()
      val rs1 = UInt(64.W)
      val rs2 = UInt(64.W)
      val rd = UInt(64.W)
      val imm = UInt(5.W)
    })
    val output = Output(new Bundle{
      val value = UInt(64.W)
      val overflow = Bool()
    })
  })

  val instructions:Seq[(PExtensionOperation.Type,(UInt,Bool))] = pext16addsub.map { case (a, b) => a -> b(io.input.rs1, io.input.rs2) } ++
    pext8addsub.map { case (a, b) => a -> b(io.input.rs1, io.input.rs2) } ++
    pext16shift.map { case (a, b) =>
      a -> b(io.input.rs1, io.input.rs2, io.input.imm)
    } ++
    pext8shift.map(a =>
      a._1 -> a._2(io.input.rs1, io.input.rs2, io.input.imm)
    ) ++
    pext16cmp.map(a => a._1 -> a._2(io.input.rs1, io.input.rs2)) ++
    pext8cmp.map(a => a._1 -> a._2(io.input.rs1, io.input.rs2)) ++
    pext16mul.map(a => a._1 -> a._2(io.input.rs1, io.input.rs2)) ++
    pext8mul.map(a => a._1 -> a._2(io.input.rs1, io.input.rs2)) ++
    pext16misc(io.input.rs1, io.input.rs2, io.input.imm) ++
    pext8misc(io.input.rs1, io.input.rs2, io.input.imm) ++
    pext8unpack(io.input.rs1) ++
    pext16pack(io.input.rs1, io.input.rs2) ++
    pextMsw32x32(io.input.rs1,io.input.rs2,io.input.rd) ++
    pextMsw32x16(io.input.rs1,io.input.rs2,io.input.rd) ++
    pextSigned16MulWith32AddSub(io.input.rs1,io.input.rs2,io.input.rd) ++
    pextSigned16MulWith64AddSub(io.input.rs1,io.input.rs2) ++
    pextMisc(io.input.rs1,io.input.rs2,io.input.rd,io.input.imm) ++
    pext8MulWith32Add(io.input.rs1,io.input.rs2,io.input.rd,io.input.imm) ++
    pext64DataComputation(io.input.rs1,io.input.rs2) ++
    pextMsw32x16(io.input.rs1,io.input.rs2,io.input.rd)

  io.output.value := MuxLookup(io.input.oeration, 0.U)(
    instructions.map { case (a, b) => a-> b._1 }
  )

  io.output.overflow := MuxLookup(io.input.oeration, 0.U)(
    instructions.map { case (a, b) => a -> b._2 }
  )
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

  def SE16(x: UInt) = SE(16)(x)

  def SE9(x: UInt) = SE(9)(x)

  def ZE(n:Int)(x:UInt) = {
    require(x.getWidth <= n)
    val t = Wire(UInt(n.W))
    t := x
    t
  }

  def ZE17(x: UInt) = ZE(17)(x)
  def ZE16(x: UInt) = ZE(17)(x)

  def ZE9(x: UInt) = ZE(9)(x)

  object SAT {

    def Q(n: Int)=(x: UInt)=>  {
      val v = x.asSInt
      val max = (pow(2, n) - 1).toInt.S
      val min = -pow(2, n).toInt.S
      (Mux(v > max, max, Mux(v < min, min, v)).asUInt,
        Mux(v > max, true.B, Mux(v < min, true.B, false.B)))
    }

    def Q31 = Q(31)
    def Q63 = Q(63)

    def Q15 = Q(15)

    def Q7 = Q(7)

    def Q(n:UInt)=(x: UInt) => {
      val v = x.asSInt
      val amt = n

      (MuxLookup(amt,0.U)((0 until pow(2,n.getWidth).toInt).map(
        i=>i.U -> {
          val max = (pow(2, i) - 1).toInt.S
          val min = -pow(2, i).toInt.S
          Mux(v > max, max, Mux(v < min, min, v)).asUInt
        }
      )),
        MuxLookup(amt, false.B)((0 until pow(2, n.getWidth).toInt).map(
          i => i.U -> {
            val max = (pow(2, i) - 1).toInt.S
            val min = -pow(2, i).toInt.S
            Mux(v > max, true.B, Mux(v < min, true.B, false.B))
          }
        )))
    }

    def U(n: UInt)=(x: UInt) => {
      val amt = n
      (MuxLookup(amt, 0.U)((0 until pow(2, n.getWidth).toInt).map(
        i => i.U -> {
          val max = (pow(2, i) - 1).toInt.U
          Mux(x > max, max, x)
        }
      )),
        MuxLookup(amt, false.B)((0 until pow(2, n.getWidth).toInt).map(
          i => i.U -> {
            val max = (pow(2, i) - 1).toInt.U
            Mux(x > max, true.B, false.B)
          }
        )))
    }

    def U(n: Int) = (x: UInt)=>  {
      val max = (pow(2, n) - 1).toInt.U
      (Mux(x > max, max, x),Mux(x > max, true.B, false.B))
    }
    def U64 = U(16)

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
      SE17(((x.asSInt >> (amt - 1.U)).asSInt + 1.S).asUInt)(16, 1)
    )
  }

  def SaturatingShiftLeft16(x: UInt, shamt: UInt) = {
    val amt = shamt(3, 0)
    val shifted = (x << amt).asUInt
    SAT.Q15(shifted)
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
      SE9(((x.asSInt >> (amt - 1.U)).asSInt + 1.S).asUInt)(8, 1)
    )
  }

  def SaturatingShiftLeft8(x: UInt, shamt: UInt) = {
    val amt = shamt(2, 0)
    val shifted = (x << amt).asUInt
    SAT.Q7(shifted)
  }

  def ROUND(x:UInt,lsb:UInt) = x + lsb

  def CLRS(x:UInt) = {
    MuxCase(0.U, (0 until x.getWidth-1).reverse.map(i=>{
      (x(x.getWidth-1) === x(i)) -> (x.getWidth-i).U
    }))
  }

  def CLZ(x: UInt) = {
    MuxCase(0.U, (0 until x.getWidth).reverse.map(i => {
      (x(i) === 0.U) -> (x.getWidth - i).U
    }))
  }
}

object PExtExecutor extends App {
  ChiselStage.emitSystemVerilogFile(
    new PExtExecutor,
    firtoolOpts = Array(
      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb"
    )
  )

}
