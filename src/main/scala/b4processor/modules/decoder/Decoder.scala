package b4processor.modules.decoder

import b4processor.Parameters
import b4processor.common.OpcodeFormat._
import b4processor.common.OpcodeFormatChecker
import b4processor.connections._
import b4processor.modules.csr.CSRAccessType
import b4processor.modules.reservationstation.ReservationStationEntry
import b4processor.structures.memoryAccess.{
  MemoryAccessInfo,
  MemoryAccessType,
  MemoryAccessWidth
}
import b4processor.utils.Tag
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

/** デコーダ
  *
  * @param instructionOffset
  *   同時に扱う命令のうちいくつ目の命令を担当するか
  * @param params
  *   パラメータ
  */
class Decoder(implicit params: Parameters)
    extends Module {
  val io = IO(new Bundle {
    val instructionFetch = Flipped(new Uncompresser2Decoder())
    val reorderBuffer = new Decoder2ReorderBuffer
    val outputCollector = Flipped(new CollectedOutput())
    val registerFile = new Decoder2RegisterFile()

    val reservationStation = new Decoder2ReservationStation

    val loadStoreQueue = Decoupled(new Decoder2LoadStoreQueue)
    val csr = Decoupled(new Decoder2CSRReservationStation())
    val threadId = Input(UInt(log2Up(params.threads).W))
  })

  // 命令からそれぞれの昨日のブロックを取り出す
  val instRd = io.instructionFetch.bits.instruction(11, 7)
  val instRs1 = io.instructionFetch.bits.instruction(19, 15)
  val instRs2 = io.instructionFetch.bits.instruction(24, 20)
  val instFunct3 = io.instructionFetch.bits.instruction(14, 12)
  val instFunct7 = io.instructionFetch.bits.instruction(31, 25)
  val instOp = io.instructionFetch.bits.instruction(6, 0)
  val instImmI = io.instructionFetch.bits.instruction(31, 20)
  val instImmS = Cat(
    io.instructionFetch.bits.instruction(31, 25),
    io.instructionFetch.bits.instruction(11, 7)
  )
  val instImmB = Cat(
    io.instructionFetch.bits.instruction(31),
    io.instructionFetch.bits.instruction(7),
    io.instructionFetch.bits.instruction(30, 25),
    io.instructionFetch.bits.instruction(11, 8)
  )
  val instImmU = io.instructionFetch.bits.instruction(31, 12)
  val instImmJ = Cat(
    io.instructionFetch.bits.instruction(31),
    io.instructionFetch.bits.instruction(19, 12),
    io.instructionFetch.bits.instruction(20),
    io.instructionFetch.bits.instruction(30, 21)
  )

  // 即値を64bitに符号拡張
  val immIExtended =
    instImmI.asSInt
  val immUExtended = Cat(instImmU, 0.U(12.W)).asSInt
  val immJExtended = Cat(instImmJ, 0.U(1.W)).asSInt

  // オペコードが何形式かを調べるモジュール
  val opcodeFormatChecker = Module(new OpcodeFormatChecker)
  opcodeFormatChecker.io.opcode := instOp

  // デスティネーションレジスタの値が使われるか(rdがない命令ではfalse)
  val destinationIsValid = opcodeFormatChecker.io.format === R ||
    opcodeFormatChecker.io.format === I ||
    opcodeFormatChecker.io.format === U ||
    opcodeFormatChecker.io.format === J

  // ソースレジスタ1に値があるかどうか
  val source1IsValid = opcodeFormatChecker.io.format === R ||
    opcodeFormatChecker.io.format === I ||
    opcodeFormatChecker.io.format === S ||
    opcodeFormatChecker.io.format === B

  // ソースレジスタ2に値があるかどうか
  val source2IsValid = opcodeFormatChecker.io.format === R ||
    opcodeFormatChecker.io.format === S ||
    opcodeFormatChecker.io.format === B

  // リオーダバッファへの入力
  io.reorderBuffer.source1.sourceRegister := Mux(source1IsValid, instRs1, 0.U)
  io.reorderBuffer.source2.sourceRegister := Mux(source2IsValid, instRs2, 0.U)
  io.reorderBuffer.destination.destinationRegister := Mux(
    destinationIsValid,
    instRd,
    0.U
  )
  io.reorderBuffer.destination.storeSign := instOp === "b0100011".U
  io.reorderBuffer.programCounter := io.instructionFetch.bits.programCounter

  // レジスタファイルへの入力
  io.registerFile.sourceRegister1 := instRs1
  io.registerFile.sourceRegister2 := instRs2

  // 各値の配置
  // ---------------------------------------
  // type || rs-val1 | rs-val2 | rs-imm
  // -----||---------|---------|------------
  // R    || rs1     | rs2     | funct7
  // I    || rs1     | pc      | imm
  // S    || rs1     | rs2     | imm
  // B    || rs1     | rs2     | imm
  // U    || imm     | pc      | 0
  // J    || imm     | pc      | 0

  // リオーダバッファから一致するタグを取得する
  // セレクタ1
  val sourceTagSelector1 = Module(new SourceTagSelector)
  sourceTagSelector1.io.reorderBufferDestinationTag <> io.reorderBuffer.source1.matchingTag
  val sourceTag1 = sourceTagSelector1.io.sourceTag
  // セレクタ2
  val sourceTagSelector2 = Module(new SourceTagSelector)
  sourceTagSelector2.io.reorderBufferDestinationTag <> io.reorderBuffer.source2.matchingTag
  val sourceTag2 = sourceTagSelector2.io.sourceTag

  // Valueの選択
  // value1
  val valueSelector1 = Module(new ValueSelector1)
  valueSelector1.io.sourceTag <> sourceTag1
  valueSelector1.io.reorderBufferValue <> io.reorderBuffer.source1.value
  valueSelector1.io.registerFileValue := io.registerFile.value1
  valueSelector1.io.outputCollector <> io.outputCollector
  valueSelector1.io.opcodeFormat := opcodeFormatChecker.io.format
  valueSelector1.io.immediateValue := MuxLookup(
    opcodeFormatChecker.io.format.asUInt,
    0.S,
    Seq(U.asUInt -> immUExtended, J.asUInt -> immJExtended)
  )
  // value2
  val valueSelector2 = Module(new ValueSelector2)
  valueSelector2.io.sourceTag <> sourceTag2
  valueSelector2.io.reorderBufferValue <> io.reorderBuffer.source2.value
  valueSelector2.io.registerFileValue := io.registerFile.value2
  valueSelector2.io.programCounter := io.instructionFetch.bits.programCounter
  valueSelector2.io.opcodeFormat := opcodeFormatChecker.io.format
  valueSelector2.io.outputCollector <> io.outputCollector

  // 命令をデコードするのはリオーダバッファにエントリの空きがあり、リザベーションステーションにも空きがあるとき
  io.instructionFetch.ready := io.reservationStation.ready && io.reorderBuffer.ready && io.loadStoreQueue.ready && io.csr.ready
  // リオーダバッファやリザベーションステーションに新しいエントリを追加するのは命令がある時
  io.reorderBuffer.valid := io.instructionFetch.ready && io.instructionFetch.valid
  io.reservationStation.entry.valid := io.instructionFetch.ready &&
    io.instructionFetch.valid &&
    !(instOp === BitPat(
      "b0?00011"
    ) && valueSelector1.io.value.valid) && instOp =/= "b1110011".U

  // RSへの出力を埋める
  val rs = io.reservationStation.entry
  rs.opcode := instOp
  rs.function3 := instFunct3
  rs.immediateOrFunction7 := MuxLookup(
    opcodeFormatChecker.io.format.asUInt,
    0.U,
    Seq(
      R.asUInt -> Cat(instFunct7, "b00000".U(5.W)),
      S.asUInt -> instImmS,
      B.asUInt -> instImmB,
      I.asUInt -> immIExtended.asUInt
    )
  )
  rs.destinationTag := io.reorderBuffer.destination.destinationTag
  rs.sourceTag1 := Mux(
    valueSelector1.io.value.valid,
    Tag(io.threadId, 0.U),
    sourceTag1.tag
  )
  rs.sourceTag2 := Mux(
    valueSelector2.io.value.valid,
    Tag(io.threadId, 0.U),
    sourceTag2.tag
  )
  rs.ready1 := valueSelector1.io.value.valid
  rs.ready2 := valueSelector2.io.value.valid
  rs.value1 := valueSelector1.io.value.bits
  rs.value2 := valueSelector2.io.value.bits
  rs.wasCompressed := io.instructionFetch.bits.wasCompressed

  // load or store命令の場合，LSQへ発送
  io.loadStoreQueue.valid := io.loadStoreQueue.ready && instOp === BitPat(
    "b0?00011"
  ) && io.instructionFetch.ready && io.instructionFetch.valid

  val STORE = "b0100011".U

  when(io.loadStoreQueue.valid) {
    io.loadStoreQueue.bits.accessInfo := MemoryAccessInfo(instOp, instFunct3)
    io.loadStoreQueue.bits.addressAndLoadResultTag := rs.destinationTag
    io.loadStoreQueue.bits.addressValid := valueSelector1.io.value.valid
    io.loadStoreQueue.bits.address := (valueSelector1.io.value.bits.asSInt + Mux(
      instOp === STORE,
      instImmS.asSInt,
      instImmI.asSInt
    )).asUInt
    when(instOp === STORE) {
      io.loadStoreQueue.bits.storeDataTag := valueSelector2.io.sourceTag.tag
      io.loadStoreQueue.bits.storeData := valueSelector2.io.value.bits
      io.loadStoreQueue.bits.storeDataValid := valueSelector2.io.value.valid
    }.otherwise {
      io.loadStoreQueue.bits.storeDataTag := Tag(io.threadId, 0.U)
      io.loadStoreQueue.bits.storeData := 0.U
      io.loadStoreQueue.bits.storeDataValid := true.B
    }
  }.otherwise {
    io.loadStoreQueue.bits := DontCare
  }

  io.csr.valid := io.csr.ready && io.instructionFetch.ready && io.instructionFetch.valid && "b1110011".U === instOp && instFunct3 =/= 0.U
  io.csr.bits := DontCare
  when(io.csr.valid) {
    io.csr.bits.csrAccessType := MuxLookup(
      instFunct3(1, 0),
      CSRAccessType.ReadWrite,
      Seq(
        "b01".U -> CSRAccessType.ReadWrite,
        "b10".U -> CSRAccessType.ReadSet,
        "b11".U -> CSRAccessType.ReadClear
      )
    )
    io.csr.bits.address := instImmI
    io.csr.bits.sourceTag := valueSelector1.io.sourceTag.tag
    io.csr.bits.value := Mux(
      instFunct3(2),
      instRs1,
      valueSelector1.io.value.bits
    )
    io.csr.bits.ready := Mux(
      instFunct3(2),
      true.B,
      valueSelector1.io.value.valid
    )
    io.csr.bits.destinationTag := io.reorderBuffer.destination.destinationTag
  }
}

object Decoder extends App {
  implicit val params = Parameters()
  (new ChiselStage).emitSystemVerilog(new Decoder)
}
