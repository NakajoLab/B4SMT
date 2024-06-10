package b4smt.connections

import b4smt.Parameters
import chisel3._
import chisel3.util._

class InstructionCache2Fetch(implicit params: Parameters) extends Bundle {
  val perDecoder = Vec(
    params.decoderPerThread,
    new Bundle {
      val request = Flipped(Valid(UInt(64.W)))
      val response = Valid(UInt(32.W))
    },
  )
  val requestNext = Flipped(Decoupled(UInt(64.W)))
}
