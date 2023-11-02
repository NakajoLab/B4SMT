package b4processor.modules.vector

import b4processor.Parameters
import chisel3._
import chisel3.util._
class VconfOutput(implicit params: Parameters) extends Bundle {
  val vtype = Valid(new VtypeBundle())
  val vl = Valid(UInt((log2Up(params.vlenb)+1).W))
}
