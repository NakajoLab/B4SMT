package b4processor.modules.executor

import b4processor.Parameters
import b4processor.common.ArithmeticOperations._
import b4processor.common.Instructions._
import b4processor.common.{
  ArithmeticOperations,
  BranchOperations,
  InstructionChecker,
  Instructions,
  OperationWidth
}
import b4processor.connections._
import b4processor.utils.Tag
import chisel3.stage.ChiselStage
import chisel3.util._
import chisel3.{Mux, _}

class Executor(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val reservationStation =
      Flipped(Irrevocable(new ReservationStation2Executor))
    val out = Irrevocable(new OutputValue)
    val fetch = Irrevocable(new BranchOutput)
  })

  /** リザベーションステーションから実行ユニットへデータを送信 op,fuctionによって命令を判断し，計算を実行
    */
  io.reservationStation.ready := io.out.ready && io.fetch.ready
  val instructionChecker = Module(new InstructionChecker)

  instructionChecker.input.opcode := io.reservationStation.bits.opcode
  instructionChecker.input.function3bits := io.reservationStation.bits.function3
  instructionChecker.input.function7bits :=
    io.reservationStation.bits.immediateOrFunction7(11, 5)

  val executionResult64bit = Wire(UInt(64.W))
  val immediateOrFunction7Extended =
    io.reservationStation.bits.immediateOrFunction7.asSInt
  val IJU_ProgramCounter = io.reservationStation.bits.value2
  val B_branchedOffset =
    (immediateOrFunction7Extended.asUInt ## 0.U(1.W)).asSInt

  io.fetch.bits.programCounterOffset := 0.S
  io.fetch.valid := false.B
  io.fetch.bits.threadId := 0.U
  io.out.valid := false.B
  executionResult64bit := 0.U

  when(io.reservationStation.valid) {
    io.out.valid := io.reservationStation.ready
    //    printf("pc=%x, immediate=%d\n", io.reservationStation.bits.programCounter, immediateOrFunction7Extended.asSInt)
    //    printf("a = %d b = %d\n", io.reservationStation.bits.value1, io.reservationStation.bits.value2)
    val a = io.reservationStation.bits.value1
    val b = Mux(
      instructionChecker.output.instruction === ArithmeticImmediate || instructionChecker.output.instruction === Load,
      immediateOrFunction7Extended,
      io.reservationStation.bits.value2.asSInt
    ).asUInt

    executionResult64bit := MuxCase(
      0.U,
      Seq(
        // ストア
        (instructionChecker.output.instruction === Instructions.Store) -> (a.asSInt + immediateOrFunction7Extended.asSInt).asUInt,
        // 加算
        (instructionChecker.output.instruction === Instructions.Load ||
          (instructionChecker.output.arithmetic === Addition)) -> (a + b),
        // 減算
        (instructionChecker.output.arithmetic === Subtraction) -> (a - b),
        // 論理積
        (instructionChecker.output.arithmetic === And) -> (a & b),
        // 論理和
        (instructionChecker.output.arithmetic === Or) -> (a | b),
        // 排他的論理和
        (instructionChecker.output.arithmetic === Xor) -> (a ^ b),
        // 左シフト
        (instructionChecker.output.arithmetic === ShiftLeftLogical) -> Mux(
          instructionChecker.output.operationWidth === OperationWidth.Word,
          a(31, 0) << b(4, 0),
          a << b(5, 0)
        ),
        // 右シフト(論理)
        (instructionChecker.output.arithmetic === ArithmeticOperations.ShiftRightLogical) -> Mux(
          instructionChecker.output.operationWidth === OperationWidth.Word,
          a(31, 0) >> b(4, 0),
          a >> b(5, 0)
        ),
        // 右シフト(算術)
        (instructionChecker.output.arithmetic === ArithmeticOperations.ShiftRightArithmetic) -> Mux(
          instructionChecker.output.operationWidth === OperationWidth.Word,
          (a(31, 0).asSInt >> b(4, 0)).asUInt,
          (a.asSInt >> b(5, 0)).asUInt
        ),
        // 比較(格納先：rd)(符号付き)
        (instructionChecker.output.arithmetic === ArithmeticOperations.SetLessThan) -> (a.asSInt < b.asSInt).asUInt,
        // 比較(格納先：rd)(符号なし)
        (instructionChecker.output.arithmetic === ArithmeticOperations.SetLessThanUnsigned) -> (a < b),
        // 無条件ジャンプ
        (instructionChecker.output.instruction === Instructions.jal || instructionChecker.output.instruction === Instructions.jalr) ->
          (IJU_ProgramCounter.asUInt +
            Mux(io.reservationStation.bits.wasCompressed, 2.U, 4.U)),
        // lui
        (instructionChecker.output.instruction === Instructions.lui) -> a,
        // auipc
        (instructionChecker.output.instruction === Instructions.auipc) -> (a.asSInt + IJU_ProgramCounter.asSInt).asUInt,
        // 分岐
        // Equal
        (instructionChecker.output.branch === BranchOperations.Equal) -> (a === b),
        // NotEqual
        (instructionChecker.output.branch === BranchOperations.NotEqual) -> (a =/= b),
        // Less Than (signed)
        (instructionChecker.output.branch === BranchOperations.LessThan) -> (a.asSInt < b.asSInt),
        // Less Than (unsigned)
        (instructionChecker.output.branch === BranchOperations.LessThanUnsigned) -> (a < b),
        // Greater Than (signed)
        (instructionChecker.output.branch === BranchOperations.GreaterOrEqual) -> (a.asSInt >= b.asSInt),
        // Greater Than (unsigned)
        (instructionChecker.output.branch === BranchOperations.GreaterOrEqualUnsigned) -> (a >= b)
      )
    )

    io.fetch.valid := ((instructionChecker.output.instruction === Instructions.Branch) ||
      (instructionChecker.output.instruction === Instructions.jalr)) && io.reservationStation.ready
    when(io.fetch.valid) {
      io.fetch.bits.threadId := io.reservationStation.bits.destinationTag.threadId
      io.fetch.bits.programCounterOffset := MuxCase(
        Mux(io.reservationStation.bits.wasCompressed, 2.S, 4.S),
        Seq(
          // 分岐
          // Equal
          (instructionChecker.output.branch === BranchOperations.Equal && a === b)
            -> B_branchedOffset,
          // NotEqual
          (instructionChecker.output.branch === BranchOperations.NotEqual && a =/= b)
            -> B_branchedOffset,
          // Less Than (signed)
          (instructionChecker.output.branch === BranchOperations.LessThan && a.asSInt < b.asSInt)
            -> B_branchedOffset,
          // Less Than (unsigned)
          (instructionChecker.output.branch === BranchOperations.LessThanUnsigned && a < b)
            -> B_branchedOffset,
          // Greater Than (signed)
          (instructionChecker.output.branch === BranchOperations.GreaterOrEqual && a.asSInt >= b.asSInt)
            -> B_branchedOffset,
          // Greater Than (unsigned)
          (instructionChecker.output.branch === BranchOperations.GreaterOrEqualUnsigned && a >= b)
            -> B_branchedOffset,
          // jalr
          (instructionChecker.output.instruction === Instructions.jalr)
            -> (Cat(
              (a.asSInt + immediateOrFunction7Extended)(63, 1),
              0.U
            ).asSInt - IJU_ProgramCounter.asSInt)
        )
      )
    }
  }

  val executionResultSized = Wire(UInt(64.W))
  executionResultSized := Mux(
    !(instructionChecker.output.instruction === Instructions.Load ||
      instructionChecker.output.instruction === Instructions.Store)
      && instructionChecker.output.operationWidth === OperationWidth.Word,
    executionResult64bit(31, 0).asSInt,
    executionResult64bit.asSInt
  ).asUInt

  //  printf("original %d\nsized %d\n", executionResult64bit, executionResultSized)

  /** 実行結果をリオーダバッファ,デコーダに送信 (validで送信データを調節) (レジスタ挿入の可能性あり)
    */

  // reorder Buffer
  //  printf(p"instruction type = ${instructionChecker.output.instruction.asUInt}\n")
  io.out.bits.resultType := Mux(
    io.reservationStation.valid &&
      instructionChecker.output.instruction =/= Instructions.Unknown &&
      instructionChecker.output.instruction =/= Instructions.Load && // load命令の場合, ReorderBufferのvalueはDataMemoryから
      instructionChecker.output.instruction =/= Instructions.Store,
    ResultType.Result,
    ResultType.LoadStoreAddress
  ) // Store命令の場合、リオーダバッファでエントリは無視される

  when(io.out.valid) {
    io.out.bits.tag := io.reservationStation.bits.destinationTag
    io.out.bits.value := executionResultSized
  }.otherwise {
    io.out.bits.tag := DontCare
    io.out.bits.value := 0.U
  }

  io.out.bits.isError := false.B
}

object Executor extends App {
  implicit val params = Parameters()
  (new ChiselStage).emitVerilog(
    new Executor(),
    args = Array(
      "--emission-options=disableMemRandomization,disableRegisterRandomization"
    )
  )
}
