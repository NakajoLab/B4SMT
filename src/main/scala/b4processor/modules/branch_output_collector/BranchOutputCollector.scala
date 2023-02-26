package b4processor.modules.branch_output_collector

import b4processor.Parameters
import b4processor.connections.{BranchOutput, CollectedOutput, OutputValue}
import b4processor.utils.FIFO
import chisel3._
import chisel3.util._

class BranchOutputCollector(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val fetch = Vec(params.threads, new CollectedBranchAddresses)
    val executor = Flipped(Vec(params.executors, Irrevocable(new BranchOutput)))
    val isError = Input(Vec(params.threads, Bool()))
  })
  private val executorQueue =
    Seq.fill(params.executors)(Module(new FIFO(2)(new BranchOutput)))
  private val executorArbiters = Seq.fill(params.threads)(
    Module(new Arbiter(new BranchOutput, params.executors))
  )
  for (i <- 0 until params.executors) {
    executorQueue(i).input <> io.executor(i)
    executorQueue(i).flush := false.B
  }
  for (tid <- 0 until params.threads) {
    for (e <- 0 until params.executors) {
      executorArbiters(tid).io.in(e) <> executorQueue(e).output
      executorArbiters(tid).io.in(e).valid :=
        executorQueue(e).output.valid &&
          executorQueue(e).output.bits.threadId === tid.U
    }
  }

  for (e <- 0 until params.executors) {
    executorQueue(e).output.ready := (0 until params.threads)
      .map(tid =>
        executorArbiters(tid).io.in(e).ready
          && executorQueue(e).output.bits.threadId === tid.U
      )
      .reduce(_ || _)
  }

  for (tid <- 0 until params.threads) {
    io.fetch(tid).addresses.bits := executorArbiters(tid).io.out.bits
    io.fetch(tid).addresses.valid := executorArbiters(tid).io.out.valid
    executorArbiters(tid).io.out.ready := true.B
    io.fetch(tid).addresses.bits.threadId := tid.U
  }

}

class CollectedBranchAddresses(implicit params: Parameters) extends Bundle {
  val addresses = Valid(new BranchOutput)
}
