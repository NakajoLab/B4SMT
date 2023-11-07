package b4processor.utils

import chisel3._
import chisel3.util._

object MuxCaseDefaultAny {
  def apply[T <: Data](seq: Seq[(Bool, T)]): T = MuxCase(seq.last._2, seq)

}
