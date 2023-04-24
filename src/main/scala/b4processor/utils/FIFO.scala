package b4processor.utils

import chisel3.stage._
import chisel3._
import chisel3.util._

import scala.math.pow

class FIFO[T <: Data](width: Int)(t: T, flow: Boolean = false) extends Module {
  val input = IO(Flipped(Decoupled(t)))
  val output = IO(Irrevocable(t))
  val full = IO(Output(Bool()))
  val empty = IO(Output(Bool()))
  val flush = IO(Input(Bool()))

  private val queue = Module(
    new Queue(
      UInt(t.getWidth.W),
      pow(2, width).toInt,
      useSyncReadMem = true,
      hasFlush = true,
      flow = flow
    )
  )
  queue.io.enq.bits := input.bits.asUInt
  queue.io.enq.valid := input.valid
  input.ready := queue.io.enq.ready

  output.valid := queue.io.deq.valid
  output.bits := queue.io.deq.bits.asTypeOf(t)
  queue.io.deq.ready := output.ready

  full := !queue.io.enq.ready
  empty := !queue.io.deq.valid
  queue.flush := flush
}

object FIFO extends App {
  (new ChiselStage).emitSystemVerilog(new FIFO(8)(new Bundle {
    val a = UInt(32.W)
  }))
}
