package b4processor.utils

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class MMArbiter[T <: Data](t: T, inputs: Int, outputs: Int) extends Module {
  val io = IO(new Bundle {
    val input = Vec(inputs, Flipped(Decoupled(t)))
    val output = Vec(outputs, Decoupled(t))
  })
  io.output <> MMArbiter(outputs, io.input)
}

object MMArbiter extends App {
  ChiselStage.emitSystemVerilogFile(new MMArbiter(UInt(4.W), 2, 1))

  def apply[T <: Data](
    outputs: Int,
    inputs: Vec[DecoupledIO[T]],
  ): Vec[DecoupledIO[T]] = {
    val t: T = inputs(0).bits.cloneType
    val inputSize = inputs.length
    val output = Wire(Vec(outputs, DecoupledIO(t)))
    // initialize
    for (o <- output) {
      o.valid := false.B
      o.bits := 0.U.asTypeOf(t)
    }

    val arbiters = (0 until outputs)
      .map(i => inputSize / outputs + (if (inputSize % outputs > i) 1 else 0))
      .filter(_ > 0)
      .map(i => Module(new Arbiter(t, i)))

    for ((in, idx) <- inputs.zipWithIndex) {
      arbiters(idx % outputs).io.in(idx / outputs) <> in
    }

    for ((out, arb) <- output.zip(arbiters)) {
      out <> arb.io.out
    }

    output
  }
}
