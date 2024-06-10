package b4smt.connections

import chisel3._
import chisel3.util.ReadyValidIO

/** 命令とデコーダをつなぐ
  */
class FetchBuffer2Uncompresser
    extends ReadyValidIO(new Bundle {
      val instruction = UInt(32.W)
      val programCounter = UInt(64.W)
    })
