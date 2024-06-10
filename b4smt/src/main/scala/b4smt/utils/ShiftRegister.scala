package b4smt.utils

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class ShiftRegister[T <: Data](t: T, width: Int, init: T) extends Module {
  val input = IO(Flipped(Valid(t)))
  val output = IO(Vec(width, t))

  val registers = RegInit(VecInit(Seq.fill(width)(init)))

  when(input.valid) {
    for (i <- 1 until width) {
      registers(i) := registers(i - 1)
    }
    registers(0) := input.bits
  }

  output := registers

  def shift(data: T): Unit = {
    input.valid := true.B
    input.bits := data
  }
}

object ShiftRegister extends App {
  ChiselStage.emitSystemVerilogFile(new ShiftRegister(UInt(32.W), 5, 0.U))

  def apply[T <: Data](t: T, width: Int, init: T): ShiftRegister[T] = {
    val m = Module(new ShiftRegister(t, width, init))
    m.input.valid := false.B
    m.input.bits := 0.U.asTypeOf(t)
    m
  }
  def apply[T <: Data](t: T, width: Int): ShiftRegister[T] =
    ShiftRegister(t, width, 0.U.asTypeOf(t))

}
