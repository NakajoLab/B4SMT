package b4processor.modules.fetch

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

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

  // オペコードを取り出す
  val opcode = io.instruction(6, 0)

  // funct3
  val funct3 = io.instruction(14, 12)

  // オフセットの抽出
  io.offset := MuxLookup(opcode(1, 0), 4.S)(
    Seq(
      "b00".U -> 2.S,
      "b01".U -> Mux(
        io.instruction(15, 13) === BitPat("b?01"),
        Cat(
          io.instruction(12),
          io.instruction(8),
          io.instruction(10, 9),
          io.instruction(6),
          io.instruction(7),
          io.instruction(2),
          io.instruction(11),
          io.instruction(5, 3),
          0.U(1.W)
        ).asSInt,
        2.S
      ),
      "b10".U -> MuxCase(
        2.S,
        Seq(
          (io.instruction(15, 13) === "b100".U &&
            io.instruction(11, 7) =/= 0.U &&
            io.instruction(6, 2) === 0.U) -> 0.S,
          (io.instruction(15, 12) === "b1001".U &&
            io.instruction(11, 7) === 0.U &&
            io.instruction(6, 2) === 0.U) -> 0.S
        )
      ),
      "b11".U -> MuxLookup(opcode, 4.S)(
        Seq(
          // jalr
          "b1100111".U -> Cat(io.instruction(31, 20), 0.U(1.W)).asSInt,
          // jal
          "b1101111".U -> Cat(
            io.instruction(31),
            io.instruction(19, 12),
            io.instruction(20),
            io.instruction(30, 21),
            0.U(1.W)
          ).asSInt,
          // branch
          "b1100011".U -> 4.S,
          // fence, fence.i
          "b0001111".U -> 4.S
        )
      )
    )
  )

  // 瓶木の種類の抽出
  io.branchType := MuxLookup(opcode(1, 0), BranchType.Next4)(
    Seq(
      "b00".U -> BranchType.Next2,
      "b01".U -> MuxCase(
        BranchType.Next2,
        Seq(
          (io.instruction(15, 13) === BitPat("b101")) -> BranchType.JAL,
          (io.instruction(15, 13) === BitPat("b11?")) -> BranchType.Branch
        )
      ),
      "b10".U -> MuxCase(
        BranchType.Next2,
        Seq(
          (io.instruction(15, 13) === "b100".U &&
            io.instruction(11, 7) =/= 0.U &&
            io.instruction(6, 2) === 0.U) -> BranchType.JALR,
          (io.instruction(15, 12) === "b1001".U &&
            io.instruction(11, 7) === 0.U &&
            io.instruction(6, 2) === 0.U) -> BranchType.Ebreak
        )
      ),
      "b11".U -> MuxLookup(opcode, BranchType.Next4)(
        Seq(
          // jalr
          "b1100111".U -> BranchType.JALR,
          // jal
          "b1101111".U -> BranchType.JAL,
          // B
          "b1100011".U -> BranchType.Branch,
          // fence, fence.i
          "b0001111".U -> Mux(
            io.instruction(12),
            BranchType.FenceI,
            BranchType.Fence
          ),
          "b1110011".U -> Mux(funct3 === 0.U, BranchType.mret, BranchType.Next4)
        )
      )
    )
  )
}

object CheckBranch extends App {
  ChiselStage.emitSystemVerilogFile(new CheckBranch)
}
