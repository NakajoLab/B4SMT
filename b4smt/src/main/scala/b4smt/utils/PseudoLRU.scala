package b4smt.utils

import chisel3._
import chisel3.util._

class PseudoLRU(depth: Int) extends Module {
  val step = IO(Input(Bool()))
  val output = IO(Output(UInt(Math.pow(2, depth).toInt.W)))

  val regs = RegInit(VecInit(Seq.fill(Math.pow(2 * 2, depth).toInt)(false.B)))

}
