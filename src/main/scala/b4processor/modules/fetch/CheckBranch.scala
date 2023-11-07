package b4processor.modules.fetch

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4processor.riscv.Instructions.{CType, IType, SYSTEMType, ZIFENCEIType}

/** フェッチモジュール用命令の種類のチェック */
class CheckBranch extends Module {
  val io = IO(new Bundle {

    /** 命令 */
    val instruction = Input(UInt(32.W))

    /** 分岐の種類 */
    val branchType = Output(BranchType())

    /** 分岐後の命令のオフセット */
    val offset = Output(SInt(21.W))
  })

  private val instruction = io.instruction

  def matchAny(input: UInt)(list: BitPat*): Bool =
    list.map(_ === input).reduce(_ || _)

  private val branchTypeAndOffset: Seq[(Bool, (BranchType.Type, SInt))] = Seq(
    (instruction === CType("C_J")) -> (BranchType.JAL, Cat(
      io.instruction(12),
      io.instruction(8),
      io.instruction(10, 9),
      io.instruction(6),
      io.instruction(7),
      io.instruction(2),
      io.instruction(11),
      io.instruction(5, 3),
      0.U(1.W),
    ).asSInt),
    (instruction === IType("JAL")) -> (BranchType.JAL, Cat(
      io.instruction(31),
      io.instruction(19, 12),
      io.instruction(20),
      io.instruction(30, 21),
      0.U(1.W),
    ).asSInt),
    (instruction === IType("JALR")) -> (BranchType.JALR, 0.S),
    (instruction === CType("C_JALR")) -> (BranchType.JALR, 0.S),
    (instruction === CType("C_JR")) -> (BranchType.JALR, 0.S),
    (matchAny(instruction)(
      IType("BEQ"),
      IType("BGE"),
      IType("BGEU"),
      IType("BLT"),
      IType("BLTU"),
      IType("BNE"),
      CType("C_BEQZ"),
      CType("C_BNEZ"),
    )) -> (BranchType.Branch, 4.S),
    (instruction === IType("FENCE")) -> (BranchType.Fence, 4.S),
    (instruction === ZIFENCEIType("FENCE_I")) -> (BranchType.FenceI, 4.S),
    (instruction === SYSTEMType("MRET")) -> (BranchType.mret, 4.S),
    (instruction === SYSTEMType("WFI")) -> (BranchType.Wfi, 4.S),
  )

  val defaultOffset =
    Mux(instruction(1, 0) === "b11".U, 4.S, 2.S)
  // 分岐のオフセット
  io.offset := MuxCase(
    defaultOffset,
    branchTypeAndOffset.map(a => a._1 -> a._2._2),
  )

  val defaultNext =
    Mux(instruction(1, 0) === "b11".U, BranchType.Next4, BranchType.Next2)
  // 分岐の種類の抽出
  io.branchType := MuxCase(
    defaultNext,
    branchTypeAndOffset.map(a => a._1 -> a._2._1),
  )
}

object CheckBranch extends App {
  ChiselStage.emitSystemVerilogFile(new CheckBranch)
}
