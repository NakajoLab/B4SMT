package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._

object PExt16Pack {
  def pext16pack = (rs1: UInt, rs2: UInt) =>
    Seq(
      // signed
      PKBB16 -> process(0, 0),
      PKBT16 -> process(0, 1),
      PKTB16 -> process(1, 0),
      PKTT16 -> process(1, 1),
    ).map(i => i._1 -> i._2(rs1, rs2))

  def process(fst: Int, snd: Int) = (rs1: UInt, rs2: UInt) => {
    val out = Seq.fill(2)(Wire(UInt(32.W)))
    for (x <- 0 until 2) {
      val ah0 = rs1.W(x).H(fst)
      val bh0 = rs2.W(x).H(snd)
      out(x) := ah0 ## bh0
    }
    (out.reverse.reduce(_ ## _), false.B)
  }

}
