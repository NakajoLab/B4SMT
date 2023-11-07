package b4processor.utils

import circt.stage.ChiselStage
import chisel3._
import chisel3.util._

import scala.math.pow

class FIFO[T <: Data](width: Int)(t: T, flow: Boolean = false) extends Module {
  val input = IO(Flipped(Decoupled(t)))
  val output = IO(Irrevocable(t))
  val full = IO(Output(Bool()))
  val empty = IO(Output(Bool()))
//  val flush = IO(Input(Bool()))

  private val queue = Module(
    new Queue(
//      UInt(t.getWidth.W),
      t,
      pow(2, width).toInt,
      useSyncReadMem = true,
//      hasFlush = true,
//      flow = flow
    ),
  )
  //  queue.io.enq.bits := input.bits.asUInt
  queue.io.enq.bits := input.bits
  queue.io.enq.valid := input.valid
  input.ready := queue.io.enq.ready

  output.valid := queue.io.deq.valid
  //  output.bits := queue.io.deq.bits.asTypeOf(t)
  output.bits := queue.io.deq.bits
  queue.io.deq.ready := output.ready

  full := !queue.io.enq.ready
  empty := !queue.io.deq.valid
//  queue.flush := flush
}

object FIFO extends App {
  ChiselStage.emitSystemVerilogFile(new FIFO(8)(new Bundle {
    val a = UInt(32.W)
  }))
}

class MyFIFO[T <: Data](t: T) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(t))
    val output = Decoupled(t)
    val empty = Output(Bool())
    val full = Output(Bool())
  })

  private val bufSizePow2 = 5

  val data = Mem(pow(2, 5).toInt, t)
  val head = RegInit(0.U(bufSizePow2.W))
  val tail = RegInit(0.U(bufSizePow2.W))

  val empty = head === tail
  val full = head + 1.U === tail

  io.output.valid := !empty
  io.output.bits := data(tail)
  when(io.output.ready && !empty) {
    tail := tail + 1.U
  }

  io.input.ready := !full
  when(io.input.valid && !full) {
    head := head + 1.U
    data(head) := io.input.bits
  }

  io.empty := empty
  io.full := full
}

object MyFIFO extends App {
  ChiselStage.emitSystemVerilogFile(
    new MyFIFO(UInt(8.W)),
    Array.empty,
    Array(
      "--disable-mem-randomization",
      "--disable-reg-randomization",
      "--disable-all-randomization",
    ),
  )
}

class MyFIFO2[T <: Data](t: T) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(t))
    val output = Decoupled(t)
    val empty = Output(Bool())
    val full = Output(Bool())
  })

  private val bufSizePow2 = 5

  val data = Mem(pow(2, bufSizePow2).toInt, t)
  val head = RegInit(0.U((bufSizePow2 + 1).W))
  val tail = RegInit(0.U((bufSizePow2 + 1).W))

  val empty = head === tail
  val full = head(bufSizePow2) =/= tail(bufSizePow2) &&
    head(bufSizePow2 - 1, 0) === tail(bufSizePow2 - 1, 0)

  io.output.valid := !empty
  io.output.bits := data(tail(bufSizePow2 - 1, 0))
  when(io.output.ready && !empty) {
    tail := tail + 1.U
  }

  io.input.ready := !full
  when(io.input.valid && !full) {
    head := head + 1.U
    data(head(bufSizePow2 - 1, 0)) := io.input.bits
  }

  io.empty := empty
  io.full := full
}

object MyFIFO2 extends App {
  ChiselStage.emitSystemVerilogFile(
    new MyFIFO2(UInt(64.W)),
    Array.empty,
    Array(
      "--disable-mem-randomization",
      "--disable-reg-randomization",
      "--disable-all-randomization",
    ),
  )
}
