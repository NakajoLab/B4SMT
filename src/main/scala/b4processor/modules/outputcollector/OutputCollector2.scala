package b4processor.modules.outputcollector

import _root_.circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.connections.{CollectedOutput, OutputValue}
import b4processor.utils.{B4RRArbiter, FIFO, MMArbiter, PassthroughBuffer}
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._

class OutputCollector2(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val outputs = Vec(params.threads, new CollectedOutput)
    val executor = Flipped(Vec(params.executors, Irrevocable(new OutputValue)))
    val dataMemory = Flipped(Irrevocable(new OutputValue))
    val csr = Flipped(Vec(params.threads, Irrevocable(new OutputValue)))
    val isError = Input(Vec(params.threads, Bool()))
  })

  val inputBuffers = Seq.fill(io.executor.length + 1)(
    Module(new PassthroughBuffer(new OutputValue))
  )

  inputBuffers.head.io.input <> io.dataMemory
  for (idx <- 0 until io.executor.length) {
    inputBuffers(idx + 1).io.input <> io.executor(idx)
  }

  val mmarb = Seq.fill(params.threads)(
    Module(
      new MMArbiter(
        new OutputValue,
        io.executor.length + 1 + 1,
        params.parallelOutput
      )
    )
  )

  for (idx <- 0 until io.executor.length + 1 + 1) {
    for (t <- 0 until params.threads) {
      mmarb(t).io.input(idx).valid := false.B
      mmarb(t).io.input(idx).bits := 0.U.asTypeOf(new OutputValue)
    }
  }

  for (t <- 0 until params.threads) {
    prefix(s"csr$t") {
      val csr_buf = Module(new PassthroughBuffer(new OutputValue))
      csr_buf.io.input <> io.csr(t)
      mmarb(t).io.input(0) <> csr_buf.io.output
    }
  }

  for ((in, idx) <- inputBuffers.zipWithIndex) {
    in.io.output.ready := false.B
    for (t <- 0 until params.threads) {
      when(in.io.output.bits.tag.threadId === t.U) {
        mmarb(t).io.input(idx + 1) <> in.io.output
      }
    }
  }

  for (t <- 0 until params.threads) {
    for (i <- 0 until params.parallelOutput) {
      io.outputs(t).outputs(i).valid := mmarb(t).io.output(i).valid
      io.outputs(t).outputs(i).bits := mmarb(t).io.output(i).bits
      mmarb(t).io.output(i).ready := true.B
    }
  }
}

object OutputCollector2 extends App {
  implicit val params = Parameters(threads = 2, executors = 4)
  ChiselStage.emitSystemVerilogFile(new OutputCollector2)
}
