package b4processor.modules.csr

import b4processor.Parameters
import b4processor.connections.{
  CSR2Fetch,
  CSRReservationStation2CSR,
  OutputValue,
  ReorderBuffer2CSR
}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4processor.riscv.CSRs
import b4processor.utils.operations.CSROperation

class CSR(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val decoderInput = Flipped(Decoupled(new CSRReservationStation2CSR))
    val CSROutput = Irrevocable(new OutputValue)
    val fetch = Output(new CSR2Fetch)
    val reorderBuffer = Flipped(new ReorderBuffer2CSR)
    val threadId = Input(UInt(log2Up(params.threads).W))
  })

  private val operation = io.decoderInput.bits.operation

  io.decoderInput.ready := io.CSROutput.ready
  io.CSROutput.valid := false.B
  io.CSROutput.bits.tag := io.decoderInput.bits.destinationTag
  io.CSROutput.bits.value := 0.U
  io.CSROutput.bits.isError := false.B

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

  def setCSROutput(reg: UInt): Unit = {
    io.CSROutput.bits.value := reg
    when(io.CSROutput.ready && io.CSROutput.valid) {
      reg := MuxLookup(operation, 0.U)(
        Seq(
          CSROperation.ReadWrite -> io.decoderInput.bits.value,
          CSROperation.ReadSet -> (reg | io.decoderInput.bits.value),
          CSROperation.ReadClear -> (reg & io.decoderInput.bits.value)
        )
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
}

object CSR extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(new CSR())
}
