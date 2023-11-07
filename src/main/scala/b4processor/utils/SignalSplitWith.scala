package b4processor.utils

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import chisel3.experimental.prefix

class SignalSplitWith[T <: Data](
  outputs: Int,
  gen: T,
  splittingLogic: T => UInt,
) extends Module
    with FormalTools {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(gen))
    val output = Vec(outputs, Decoupled(gen))
  })

  io.output <> SignalSplitWith(outputs, io.input)(splittingLogic)
}

object SignalSplitWith extends App with FormalTools {
  var globalIndex = 0
  def apply[T <: Data](outputs: Int, input: ReadyValidIO[T])(
    splittingLogic: T => UInt,
  ): Vec[DecoupledIO[T]] = {
    val t: T = input.bits.cloneType
    val out = Wire(Vec(outputs, Decoupled(t)))

    val index = splittingLogic(input.bits)
    for ((o, i) <- out.zipWithIndex) {
      prefix(s"index$i") {
        val correctIndex = index === i.U
        o.bits := Mux(correctIndex, input.bits, 0.U.asTypeOf(t))
        o.valid := Mux(correctIndex, input.valid, false.B)
      }
    }
    input.ready := out(index).ready

    cover(input.ready)
    out map (_.valid) foreach {
      cover(_)
    }
    when(input.valid) {
      val valid_count =
        out map (_.valid) map (0.U(10.W) ## _.asUInt) reduce (_ + _)
      assert(valid_count === 1.U, s"${globalIndex}")
    }
    globalIndex += 1
    out
  }

  ChiselStage.emitSystemVerilogFile(
    new SignalSplitWith[UInt](4, UInt(4.W), _(1, 0)),
  )
}
