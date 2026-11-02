package b4smt.modules.decoder

import b4smt.Parameters
import b4smt.connections._
import b4smt.utils.{Tag, TagValueBundle}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4smt.modules.Decoder2AtomicLSU
import b4smt.modules.reservationstation.{ExecutorType, ReservationStationEntry}
import b4smt.utils.operations.{DecodingMod, LoadStoreOperation}
import chiselformal.FormalTools

/** Decoder
  */
class Decoder(implicit params: Parameters) extends Module with FormalTools {
  val io = IO(new Bundle {
    val threadId = Input(UInt(log2Up(params.threads).W))

    val instructionFetch = Flipped(new Uncompresser2Decoder())
    val reorderBuffer = new Decoder2ReorderBuffer
    val outputCollector = Flipped(new CollectedOutput())
    val registerFile = new Decoder2RegisterFile()

    val reservationStation = new Decoder2ReservationStation
    val loadStoreQueue = Decoupled(new Decoder2LoadStoreQueue)
    val csr = Decoupled(new Decoder2CSRReservationStation())
    val amo = Decoupled(new Decoder2AtomicLSU)
  })

  val operations = DecodingMod(
    io.instructionFetch.bits.instruction,
    io.instructionFetch.bits.programCounter,
  )

  io.reorderBuffer.isDecodeError := io.instructionFetch.valid && !operations.valid

  val operationInorder =
    LoadStoreOperation.Store === operations.loadStoreOp.validDataOrZero ||
      operations.amoOp.isValid ||
      operations.csrOp.isValid

  // Inputs to the reorder buffer
  io.reorderBuffer.sources zip operations.sources foreach { case (rob, o) =>
    rob.sourceRegister := o.reg
  }
  io.reorderBuffer.destination.destinationRegister := operations.rd
  io.reorderBuffer.destination.operationInorder := operationInorder
  io.reorderBuffer.programCounter := io.instructionFetch.bits.programCounter

  // Inputs to the register file
  io.registerFile.sourceRegisters zip operations.sources foreach {
    case (rf, o) => rf := o.reg
  }

  // Get matching tags from the reorder buffer
  val sourceTags = Wire(Vec(3, Valid(new Tag)))
  sourceTags := io.reorderBuffer.sources.map(_.matchingTag)
  if (!params.enablePExt) {
    sourceTags(2).valid := false.B
    sourceTags(2).bits := Tag(io.threadId, 0.U)
  }
  sourceTags.foreach(s => s.bits.threadId.foreach(_ := io.threadId))
  val destinationTag = io.reorderBuffer.destination.destinationTag

  // Select Value
  val values = Wire(Vec(3, Valid(UInt(64.W))))
  values := io.reorderBuffer.sources zip io.registerFile.values zip sourceTags map {
    case ((rob, rf), st) =>
      ValueSelector.getValue(rob.value, rf, io.outputCollector, st)
  }
  if (!params.enablePExt)
    values(2) := 0.U.asTypeOf(Valid(UInt(64.W)))

  // Decode instructions when both the reorder buffer and reservation station have free entries
  io.instructionFetch.ready := io.reservationStation.ready && io.reorderBuffer.ready && io.loadStoreQueue.ready && io.csr.ready && io.amo.ready
  // Add new entries to the reorder buffer and reservation station when an instruction is available
  io.reorderBuffer.valid := io.instructionFetch.fire

  // Populate outputs to the RS
  val rs = io.reservationStation.entry
  rs := 0.U.asTypeOf(new ReservationStationEntry)
  rs.valid := io.instructionFetch.fire &&
    (operations.aluOp.isValid || operations.pextOp.isValid)
  when(rs.valid) {
    rs.operation := operations.aluOp.validDataOrZero
    rs.pextOperation := operations.pextOp.validDataOrZero
    rs.exeType := MuxCase(
      ExecutorType.Regular,
      Seq(
        operations.mOp.valid -> ExecutorType.MulDiv,
        operations.pextOp.valid -> ExecutorType.PExt,
      ),
    )
    rs.destinationTag := destinationTag
    rs.sources zip values zip sourceTags zip operations.sources foreach {
      case (((rs_s, v), st), o) =>
        rs_s := TagValueBundle.fromConditional(
          !(v.valid || o.valid),
          st.bits,
          MuxCase(0.U, Seq(o.valid -> o.value, v.valid -> v.bits)),
        )
    }

    rs.branchOffset := operations.branchOffset
    rs.wasCompressed := io.instructionFetch.bits.wasCompressed
  }

  // Dispatch load or store instructions to the LSQ
  io.loadStoreQueue.bits := 0.U.asTypeOf(new Decoder2LoadStoreQueue)
  io.loadStoreQueue.valid := io.loadStoreQueue.ready && io.instructionFetch.fire && operations.loadStoreOp.isValid
  when(io.loadStoreQueue.valid) {
    io.loadStoreQueue.bits.operation := operations.loadStoreOp.validDataOrZero
    io.loadStoreQueue.bits.operationWidth := operations.loadStoreWidth
    io.loadStoreQueue.bits.destinationTag := destinationTag
    io.loadStoreQueue.bits.addressValid := values(0).valid
    io.loadStoreQueue.bits.address := values(0).bits
    io.loadStoreQueue.bits.addressTag := sourceTags(0).bits
    io.loadStoreQueue.bits.addressOffset := operations
      .sources(1)
      .value(11, 0)
      .asSInt
    when(operationInorder) {
      io.loadStoreQueue.bits.storeDataTag := sourceTags(1).bits
      io.loadStoreQueue.bits.storeData := values(1).bits
      io.loadStoreQueue.bits.storeDataValid := values(1).valid
    }.otherwise {
      io.loadStoreQueue.bits.storeDataTag := Tag(io.threadId, 0.U)
      io.loadStoreQueue.bits.storeData := 0.U
      io.loadStoreQueue.bits.storeDataValid := true.B
    }
  }
  // thread ids are constant
  io.loadStoreQueue.bits.destinationTag.threadId.foreach(_ := io.threadId)
  io.loadStoreQueue.bits.storeDataTag.threadId.foreach(_ := io.threadId)
  io.loadStoreQueue.bits.addressTag.threadId.foreach(_ := io.threadId)

  io.csr.valid := io.csr.ready && io.instructionFetch.fire & operations.csrOp.isValid
  io.csr.bits := 0.U.asTypeOf(new Decoder2CSRReservationStation)
  when(io.csr.valid) {
    io.csr.bits.operation := operations.csrOp.validDataOrZero
    io.csr.bits.address := operations.sources(1).value
//    printf(p"decoder out ${operations.csrOp}\n")
    io.csr.bits.sourceTag := sourceTags(0).bits
    io.csr.bits.value := MuxCase(
      0.U,
      Seq(
        operations.sources(0).valid -> operations.sources(0).value,
        values(0).valid -> values(0).bits,
      ),
    )
    io.csr.bits.ready := operations.sources(0).valid || values(0).valid
    io.csr.bits.destinationTag := io.reorderBuffer.destination.destinationTag
  }
  io.csr.bits.sourceTag.threadId.foreach(_ := io.threadId)
  io.csr.bits.destinationTag.threadId.foreach(_ := io.threadId)

  io.amo.valid := io.amo.ready && io.instructionFetch.fire && operations.amoOp.isValid
  io.amo.bits := 0.U.asTypeOf(new Decoder2AtomicLSU)
  when(io.amo.valid) {
    io.amo.bits.valid := true.B
    io.amo.bits.operation := operations.amoOp.validDataOrZero
    io.amo.bits.operationWidth := operations.amoWidth
    io.amo.bits.ordering := operations.amoOrdering

    io.amo.bits.addressReg := Mux(
      sourceTags(0).valid,
      sourceTags(0).bits,
      Tag(io.threadId, 0.U),
    )
    io.amo.bits.addressValue := values(0).bits
    io.amo.bits.addressValid := values(0).valid

    io.amo.bits.srcReg := Mux(
      sourceTags(1).valid,
      sourceTags(1).bits,
      Tag(io.threadId, 0.U),
    )
    io.amo.bits.srcValue := values(1).bits
    io.amo.bits.srcValid := values(1).valid

    io.amo.bits.destinationTag := destinationTag
  }
  // thread ids are const
  io.amo.bits.srcReg.threadId.foreach(_ := io.threadId)
  io.amo.bits.addressReg.threadId.foreach(_ := io.threadId)
  io.amo.bits.destinationTag.threadId.foreach(_ := io.threadId)

  // FORMAL
  takesEveryValue(io.reorderBuffer.valid)
  takesEveryValue(io.reservationStation.entry.valid)
  takesEveryValue(io.loadStoreQueue.valid)
  takesEveryValue(io.csr.valid)
  takesEveryValue(io.amo.valid)

  // source and destination tags threadid should be const
  // TODO: fix
//  rs.sources.zipWithIndex foreach { case (s, i) =>
//    when(s.isTag) {
//      assert(
//        s.getTagUnsafe.getThreadId === io.threadId,
//        s"thread id wrong on source($i)",
//      )
//    }
//  }
//  assert(
//    io.amo.bits.srcReg.getThreadId === io.threadId,
//    "amo source thread id wrong",
//  )
//  assert(
//    io.amo.bits.destinationTag.getThreadId === io.threadId,
//    "amo destination thread id wrong",
//  )
//  assert(io.csr.bits.sourceTag.getThreadId === io.threadId)
//  assert(io.csr.bits.destinationTag.getThreadId === io.threadId)
//  assert(io.loadStoreQueue.bits.destinationTag.getThreadId === io.threadId)
//  assert(io.loadStoreQueue.bits.storeDataTag.getThreadId === io.threadId)
//  assert(io.loadStoreQueue.bits.addressTag.getThreadId === io.threadId)

  // assumptions
  assume(stable(io.threadId))
//  io.reorderBuffer.sources.zipWithIndex foreach { case (s, i) =>
//    assume(
//      s.matchingTag.bits.getThreadId === io.threadId,
//      s"reorder buffer source $i threadid should be the same as decoder"
//    )
//  }
//  assume(
//    io.reorderBuffer.destination.destinationTag.getThreadId === io.threadId,
//    "reorder buffer destination threadid should be the same as decoder"
//  )
}

object Decoder extends App {
  implicit val params: b4smt.Parameters = Parameters(enablePExt = false)
  ChiselStage.emitSystemVerilogFile(new Decoder)
}
