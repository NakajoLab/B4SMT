package b4processor.modules.outputcollector

import b4processor.Parameters
import b4processor.connections.{CollectedOutput, OutputValue}
import b4processor.utils.{B4RRArbiter, FIFO}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class OutputCollector(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val outputs = Vec(params.threads, new CollectedOutput)
    val executor = Flipped(Vec(params.executors, Irrevocable(new OutputValue)))
    val dataMemory = Flipped(Irrevocable(new OutputValue))
    val csr = Flipped(Vec(params.threads, Irrevocable(new OutputValue)))
    val isError = Input(Vec(params.threads, Bool()))
  })

  val executorQueue =
    Seq.fill(params.executors)(Module(new FIFO(2)(new OutputValue)))
  val threadsArbiter =
    Seq.fill(params.threads)(
      Module(new B4RRArbiter(new OutputValue, params.executors + 2))
    )

  for (i <- 0 until params.executors) {
    executorQueue(i).input <> io.executor(i)
    executorQueue(i).flush := false.B

    val out = io.executor(i).bits
    val outValid = io.executor(i).valid
    val outReady = io.executor(i).ready
    // No Same input
    if (params.debug)
      when(outValid && RegNext(outValid)) {
        assert(outReady && RegNext(outReady) && !(out === RegNext(out)))
      }
  }

  for (tid <- 0 until params.threads) {
    for (i <- 0 until params.executors) {
      threadsArbiter(tid).io.in(i) <> executorQueue(i).output
      threadsArbiter(tid).io.in(i).valid :=
        executorQueue(i).output.valid &&
          executorQueue(i).output.bits.tag.threadId === tid.U
    }
    threadsArbiter(tid).io.in(params.executors) <> io.dataMemory
    threadsArbiter(tid).io
      .in(params.executors)
      .valid := io.dataMemory.valid && io.dataMemory.bits.tag.threadId === tid.U
    threadsArbiter(tid).io.in(params.executors + 1) <> io.csr(tid)

    io.outputs(tid).outputs(0).bits := threadsArbiter(tid).io.out.bits
    io.outputs(tid).outputs(0).valid := threadsArbiter(tid).io.out.valid
    threadsArbiter(tid).io.out.ready := true.B

    for (i <- 1 until params.parallelOutput) {
      io.outputs(tid).outputs(i).valid := false.B
      io.outputs(tid).outputs(i).bits := 0.U
    }

    val out = threadsArbiter(tid).io.out.bits
    val outValid = threadsArbiter(tid).io.out.valid
    val outReady = threadsArbiter(tid).io.out.ready
    // No Same input
    if (params.debug)
      when(outValid && RegNext(outValid)) {
        assert(outReady && RegNext(outReady) && !(out === RegNext(out)))
      }
  }

  for (i <- 0 until params.executors) {
    executorQueue(i).output.ready := (0 until params.threads)
      .map(tid =>
        threadsArbiter(tid).io
          .in(i)
          .ready && executorQueue(i).output.bits.tag.threadId === tid.U
      )
      .reduce(_ || _)
  }
  io.dataMemory.ready := (0 until params.threads)
    .map(tid =>
      threadsArbiter(tid).io
        .in(params.executors)
        .ready && io.dataMemory.bits.tag.threadId === tid.U
    )
    .reduce(_ || _)
}

object OutputCollector extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(new OutputCollector)
}
