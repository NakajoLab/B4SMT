package b4smt_pext

import _root_.circt.stage.ChiselStage
import PExt16AddSub.pext16addsub
import PExt16Compare.pext16cmp
import PExt16Misc.pext16misc
import PExt16Multiply.pext16mul
import PExt16Pack.pext16pack
import PExt16Shift.pext16shift
import PExt32Computation.pext32Computation
import PExt32MulWith64AddSub.pextMsw32x16
import PExt64DataComputation.pext64DataComputation
import PExt64_32AddSub.pext32addsub
import PExt64_32Misc.pext32misc
import PExt64_32Multiply.pext32mul
import PExt64_32MultiplyAndAdd.pext64_32MultiplyAndAdd
import PExt64_32ParallelMultiplyAndAdd.pext64_32ParallelMultiplyAndAdd
import PExt64_32Shift.pext32shift
import PExt64_32packing.pext64_32packing
import PExt64_NonSIMD32Shift.pext64_NonSIMD32Shift
import PExt64_Q15.pextQ15
import PExt8AddSub.pext8addsub
import PExt8Compare.pext8cmp
import PExt8Misc.pext8misc
import PExt8MulWith32Add.pext8MulWith32Add
import PExt8Multiply.pext8mul
import PExt8Shift.pext8shift
import PExt8Unpack.pext8unpack
import PExtMSW32x32MulAdd.pextMsw32x32
import PExtMisc.pextMisc
import PExtMisc2.pextMisc2
import PExtQ16Saturate.pextQ16Saturate
import PExtQ32Saturate.pextQ32Saturate
import PExtSigned16MulWith32AddSub.pextSigned16MulWith32AddSub
import PExtSigned16MulWith64AddSub.pextSigned16MulWith64AddSub
import chisel3._
import chisel3.util._

import scala.math.pow

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
      pextSigned16MulWith64AddSub(io.input.rs1, io.input.rs2, io.input.rd) ++
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

object PExtExecutor extends App {
  ChiselStage.emitSystemVerilogFile(
    new PExtExecutor,
    firtoolOpts = Array(
      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb",
    ),
  )
}
