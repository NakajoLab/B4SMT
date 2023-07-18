package b4processor.utils

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class PassthroughBuffer[T <: Data](t: T) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(t))
    val output = Decoupled(t)
  })

  val buf = Reg(t)
  val bufSet = RegInit(false.B)

  io.output.valid := false.B
  io.output.bits := 0.U.asTypeOf(t)
  io.input.ready := false.B

  when(!bufSet) {
    io.input.ready := true.B
    when(io.input.valid) {
      io.output.valid := true.B
      io.output.bits := io.input.bits
      when(!io.output.ready) {
        buf := io.input.bits
        bufSet := true.B
      }
    }
  }.otherwise {
    io.output.valid := true.B
    io.output.bits := buf
    when(!io.input.valid && io.output.ready) {
      buf := 0.U.asTypeOf(t)
      bufSet := false.B
    }
  }
}

object PassthroughBuffer extends App {
  ChiselStage.emitSystemVerilogFile(
    new PassthroughBuffer(UInt(4.W)),
    Array.empty,
    Array(
      "--disable-mem-randomization",
      "--disable-reg-randomization",
      "--disable-all-randomization"
    )
  )
}
