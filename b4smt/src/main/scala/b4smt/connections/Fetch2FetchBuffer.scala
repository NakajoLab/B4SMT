package b4smt.connections

import b4smt.Parameters
import chisel3._
import chisel3.util._

/** Connects instructions and the decoder
  */
class Fetch2FetchBuffer(implicit params: Parameters) extends Bundle {
  val toBuffer = Vec(
    params.decoderPerThread,
    Decoupled(new Bundle {
      val instruction = UInt(32.W)
      val programCounter = UInt(64.W)
    }),
  )
  val empty = Input(Bool())
}
