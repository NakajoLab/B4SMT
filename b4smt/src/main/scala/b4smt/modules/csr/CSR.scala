package b4smt.modules.csr

import b4smt.Parameters
import b4smt.connections.{
  CSR2Fetch,
  CSRReservationStation2CSR,
  OutputValue,
  ReorderBuffer2CSR,
}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4smt.riscv.CSRs
import b4smt.utils.operations.CSROperation
import chiselformal.FormalTools

class CSR(implicit params: Parameters) extends Module with FormalTools {
  val io = IO(new Bundle {
    val decoderInput = Flipped(Decoupled(new CSRReservationStation2CSR))
    val CSROutput = Irrevocable(new OutputValue)
    val fetch = Output(new CSR2Fetch)
    val reorderBuffer = Flipped(new ReorderBuffer2CSR)
    val threadId = Input(UInt(log2Up(params.threads).W))
    val customInt = Valid(UInt(params.threads.W))

    // ---------------------- status inputs------------------------
    // executors
    val activeExecutor = Input(UInt(log2Ceil(params.executors + 1).W))
    // load store
    val activeLoad = Input(Bool())
    val activeStore = Input(Bool())
    val fullLoadStore = Input(Bool())
    // A ext
    val activeAmo = Input(Bool())
    val activeLr = Input(Bool())
    val activeSc = Input(Bool())
    val activeScFail = Input(Bool())
    val fullAmo = Input(Bool())
    // error
    val activeError = Input(Bool())
    // P ext
    val activePext = Input(UInt(log2Ceil(params.pextExecutors + 1).W))
    // reorder buffer
    val fullReorderBuffer = Input(Bool())
    // reservation station
    val fullReservationStation =
      Input(UInt(log2Ceil(params.decoderPerThread + 1).W))
    // ------------------------------------------------------------
  })

  private val operation = io.decoderInput.bits.operation

  io.decoderInput.ready := io.CSROutput.ready
  io.CSROutput.valid := false.B
  io.CSROutput.bits.tag := io.decoderInput.bits.destinationTag
  io.CSROutput.bits.value := 0.U
  io.CSROutput.bits.isError := false.B
  io.customInt.valid := false.B
  io.customInt.bits := 0.U

  val retireCounter = Module(new RetireCounter)
  retireCounter.io.retireInCycle := io.reorderBuffer.retireCount
  val cycleCounter = Module(new CycleCounter)

  val mtvec = RegInit(0.U(64.W))
  io.fetch.mtvec := mtvec
  val mepc = RegInit(0.U(64.W))
  io.fetch.mepc := mepc
  val mcause = RegInit(0.U(64.W))
  io.fetch.mcause := mcause
  val mstatus = RegInit(0.U(64.W))
  val mie = RegInit(0.U(64.W))

  val executorCount = RegInit(0.U(64.W))
  executorCount := executorCount + io.activeExecutor
  val loadCount = RegInit(0.U(64.W))
  loadCount := loadCount + io.activeLoad
  val storeCount = RegInit(0.U(64.W))
  storeCount := storeCount + io.activeStore
  val amoCount = RegInit(0.U(64.W))
  amoCount := amoCount + io.activeAmo
  val lrCount = RegInit(0.U(64.W))
  lrCount := lrCount + io.activeLr
  val scCount = RegInit(0.U(64.W))
  scCount := scCount + io.activeSc
  val scFailCount = RegInit(0.U(64.W))
  scFailCount := scFailCount + io.activeScFail
  val errorCount = RegInit(0.U(64.W))
  errorCount := errorCount + io.activeError
  val pextCount = RegInit(0.U(64.W))
  pextCount := pextCount + io.activePext
  val loadStoreFullCount = RegInit(0.U(64.W))
  loadStoreFullCount := loadStoreFullCount + io.fullLoadStore
  val atomicFullCount = RegInit(0.U(64.W))
  atomicFullCount := atomicFullCount + io.fullAmo
  val reorderBufferFullCount = RegInit(0.U(64.W))
  reorderBufferFullCount := reorderBufferFullCount + io.fullReorderBuffer
  val reservationStationFullCount = RegInit(0.U(64.W))
  reservationStationFullCount := reservationStationFullCount + io.fullReservationStation

  def setCSROutput(reg: UInt): Unit = {
    io.CSROutput.bits.value := reg
    when(io.CSROutput.ready && io.CSROutput.valid) {
      reg := MuxLookup(operation, 0.U)(
        Seq(
          CSROperation.ReadWrite -> io.decoderInput.bits.value,
          CSROperation.ReadSet -> (reg | io.decoderInput.bits.value),
          CSROperation.ReadClear -> (reg & io.decoderInput.bits.value),
        ),
      )
    }
  }

  when(io.decoderInput.valid) {
//    printf(p"csr in ${operation}\n")
    val address = io.decoderInput.bits.address
//    printf(p"csr address ${address}\n")

    io.CSROutput.valid := true.B

    when(address === CSRs.cycle.U || address === CSRs.mcycle.U) {
      io.CSROutput.bits.value := cycleCounter.count
    }.elsewhen(address === CSRs.instret.U || address === CSRs.minstret.U) {
      io.CSROutput.bits.value := retireCounter.io.count
    }.elsewhen(address === CSRs.mhartid.U) {
      io.CSROutput.bits.value := io.threadId
    }.elsewhen(address === CSRs.mtvec.U) {
      setCSROutput(mtvec)
    }.elsewhen(address === CSRs.mepc.U) {
      setCSROutput(mepc)
    }.elsewhen(address === CSRs.mcause.U) {
      setCSROutput(mcause)
    }.elsewhen(address === CSRs.mstatus.U) {
      setCSROutput(mstatus)
    }.elsewhen(address === CSRs.mie.U) {
      setCSROutput(mie)
    }.elsewhen(address === CSRs.hpmcounter3.U) {
      io.CSROutput.bits.value := executorCount
    }.elsewhen(address === CSRs.hpmcounter4.U) {
      io.CSROutput.bits.value := loadCount
    }.elsewhen(address === CSRs.hpmcounter5.U) {
      io.CSROutput.bits.value := storeCount
    }.elsewhen(address === CSRs.hpmcounter6.U) {
      io.CSROutput.bits.value := amoCount
    }.elsewhen(address === CSRs.hpmcounter7.U) {
      io.CSROutput.bits.value := lrCount
    }.elsewhen(address === CSRs.hpmcounter8.U) {
      io.CSROutput.bits.value := scCount
    }.elsewhen(address === CSRs.hpmcounter9.U) {
      io.CSROutput.bits.value := scFailCount
    }.elsewhen(address === CSRs.hpmcounter10.U) {
      io.CSROutput.bits.value := errorCount
    }.elsewhen(address === CSRs.hpmcounter11.U) {
      io.CSROutput.bits.value := pextCount
    }.elsewhen(address === CSRs.hpmcounter12.U) {
      io.CSROutput.bits.value := reorderBufferFullCount
    }.elsewhen(address === CSRs.hpmcounter13.U) {
      io.CSROutput.bits.value := loadStoreFullCount
    }.elsewhen(address === CSRs.hpmcounter14.U) {
      io.CSROutput.bits.value := atomicFullCount
    }.elsewhen(address === CSRs.hpmcounter15.U) {
      io.CSROutput.bits.value := reservationStationFullCount
    }.elsewhen(address === CSRs.misa.U) {
      io.CSROutput.bits.value := MisaGen()
    }.elsewhen(address === CSRs.mvendorid.U) {
      io.CSROutput.bits.value := 0.U
    }.elsewhen(address === CSRs.marchid.U) {
      io.CSROutput.bits.value := 0.U
    }.elsewhen(address === CSRs.mimpid.U) {
      io.CSROutput.bits.value := 0.U
    }.elsewhen(address === CSRCustom.coreCount.U) {
      io.CSROutput.bits.value := params.threads.U
    }.elsewhen(address === CSRCustom.customInt.U) {
      io.CSROutput.bits.value := 0.U
      when(io.CSROutput.ready && io.CSROutput.valid) {
        io.customInt.valid := true.B
        io.customInt.bits := io.decoderInput.bits.value
      }
    }.otherwise {
      io.CSROutput.bits.isError := true.B
    }
  }

  when(io.reorderBuffer.mcause.valid) {
    mcause := io.reorderBuffer.mcause.bits
  }
  when(io.reorderBuffer.mepc.valid) {
    mepc := io.reorderBuffer.mepc.bits
  }
  io.CSROutput.bits.tag.threadId := io.threadId

  when(io.decoderInput.valid) {
    assume(io.decoderInput.bits.destinationTag.threadId === io.threadId)
  }
  when(io.CSROutput.valid) {
    assert(io.CSROutput.bits.tag.threadId === io.threadId)
  }
  takesEveryValue(io.CSROutput.valid)
}

object CSR extends App {
  implicit val params: b4smt.Parameters = Parameters()
  ChiselStage.emitSystemVerilogFile(new CSR())
}
