package b4smt.connections

import chisel3._
import chisel3.util.ReadyValidIO

/** Connects instructions and the decoder
  */
class Uncompresser2Decoder
    extends ReadyValidIO(new Bundle {
      val instruction = UInt(32.W)
      val programCounter = UInt(64.W)
      val wasCompressed = Bool()
    })
