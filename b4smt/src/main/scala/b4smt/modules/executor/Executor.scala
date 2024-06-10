package b4smt.modules.executor

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4smt.Parameters
import b4smt.connections._
import b4smt.utils.Tag
import chisel3.experimental.prefix

class Executor(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val reservationStation =
      Flipped(Irrevocable(new ReservationStation2Executor))
    val out = Irrevocable(new OutputValue)
    val fetch = Irrevocable(new BranchOutput)
    val status = Output(UInt(params.threads.W))
  })

  private val operation = io.reservationStation.bits.operation

  /** リザベーションステーションから実行ユニットへデータを送信 op,fuctionによって命令を判断し，計算を実行
    */
  io.reservationStation.ready := io.out.ready && io.fetch.ready

  io.fetch.bits.programCounterOffset := 0.S
  io.fetch.valid := false.B
  io.fetch.bits.threadId := 0.U
  io.out.valid := false.B
  io.out.bits.value := 0.U
  io.out.bits.tag := Tag(0, 0)
  io.out.bits.isError := false.B
  io.status := 0.U

  val a = io.reservationStation.bits.value1
  val b = io.reservationStation.bits.value2

  import b4smt.utils.operations.ALUOperation._

  val nextOffset =
    Mux(io.reservationStation.bits.wasCompressed, 2.S, 4.S)

  val executeOutput = Wire(UInt(64.W))
  prefix("execute_output_tmp") {
    executeOutput :=
      MuxLookup(operation, 0.U)(
        Seq(
          Add -> (a + b),
          Sub -> (a - b),
          And -> (a & b),
          Or -> (a | b),
          Xor -> (a ^ b),
          Slt -> (a.asSInt < b.asSInt).asUInt,
          Sltu -> (a < b),
          Sll -> (a << b(5, 0)).asUInt,
          Srl -> (a >> b(5, 0)).asUInt,
          Sra -> (a.asSInt >> b(5, 0)).asUInt,
          AddJALR -> (a + b),
          AddW -> (a(31, 0).asSInt + b(31, 0).asSInt).pad(64).asUInt,
          SllW -> (a(31, 0) << b(4, 0))(31, 0).asSInt.pad(64).asUInt,
          SrlW -> (a(31, 0) >> b(4, 0))(31, 0).asSInt.pad(64).asUInt,
          SraW -> (a(31, 0).asSInt >> b(4, 0)).asSInt.pad(64).asUInt,
          SubW -> (a(31, 0).asSInt - b(31, 0).asSInt).pad(64).asUInt,
          AddJAL -> (b + nextOffset.asUInt),
          AddJALR -> (b + nextOffset.asUInt),
          Mul -> (a.asSInt * b.asSInt)(63, 0).asUInt,
          Mulh -> (a.asSInt * b.asSInt)(127, 64).asUInt,
          Mulhu -> (a * b)(127, 64),
          Mulhsu -> (a.asSInt * b)(127, 64).asUInt,
          Mulw -> (a(31, 0).asSInt * b(31, 0).asSInt).asUInt,
        ),
      )
  }

  val isBranch = Seq(
    BranchEqual,
    BranchNotEqual,
    BranchLessThan,
    BranchLessThanUnsigned,
    BranchGreaterThanOrEqual,
    BranchGreaterThanOrEqualUnsigned,
  ).map(_ === operation).reduce(_ || _)
  val branchedOffset = MuxCase(
    0.S,
    Seq(
      isBranch -> (io.reservationStation.bits.branchOffset ## 0.S(1.W)).asSInt,
      (operation === AddJALR) -> (a.asSInt - b.asSInt + io.reservationStation.bits.branchOffset),
    ),
  )

  val fetchOffset = Wire(SInt(64.W))
  prefix("fetch_output_tmp") {
    fetchOffset := MuxLookup(operation, 0.S)(
      Seq(
        BranchEqual -> Mux(a === b, branchedOffset, nextOffset),
        BranchNotEqual -> Mux(a =/= b, branchedOffset, nextOffset),
        BranchLessThan ->
          Mux(a.asSInt < b.asSInt, branchedOffset, nextOffset),
        BranchGreaterThanOrEqual ->
          Mux(a.asSInt >= b.asSInt, branchedOffset, nextOffset),
        BranchLessThanUnsigned ->
          Mux(a < b, branchedOffset, nextOffset),
        BranchGreaterThanOrEqualUnsigned ->
          Mux(a >= b, branchedOffset, nextOffset),
        AddJALR -> branchedOffset,
      ),
    )
  }

  io.fetch.valid := io.reservationStation.valid && io.reservationStation.ready && (isBranch || io.reservationStation.bits.operation === AddJALR)
  io.fetch.bits.programCounterOffset := fetchOffset
  io.fetch.bits.threadId := io.reservationStation.bits.destinationTag.threadId

  when(io.reservationStation.valid && io.reservationStation.ready) {
    io.out.valid := io.reservationStation.ready
    io.out.bits.value := executeOutput
    io.out.bits.tag := io.reservationStation.bits.destinationTag
  }

  when(io.out.valid && io.out.ready) {
    io.status := 1.U << io.out.bits.tag.threadId
  }

  io.out.bits.isError := false.B
}

object Executor extends App {
  implicit val params: b4smt.Parameters = Parameters()
  ChiselStage.emitSystemVerilogFile(new Executor())
}
