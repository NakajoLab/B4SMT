package b4processor.modules.PExt

import b4processor.modules.PExt.PExtensionOperation._
import b4processor.modules.PExt.UIntSectionHelper._
import chisel3._
import chisel3.util.MuxCase

object PExt8Unpack {
  def pext8unpack = (rs1: UInt) =>
    Seq(
      // signed
      SUNPKD810 -> processS(Seq((3, 5), (2, 4), (1, 1), (0, 0))),
      SUNPKD820 -> processS(Seq((3, 6), (2, 4), (1, 2), (0, 0))),
      SUNPKD830 -> processS(Seq((3, 7), (2, 4), (1, 3), (0, 0))),
      SUNPKD831 -> processS(Seq((3, 7), (2, 5), (1, 3), (0, 1))),
      SUNPKD832 -> processS(Seq((3, 7), (2, 6), (1, 3), (0, 2))),
      // unsigned
      ZUNPKD810 -> processZ(Seq((3, 5), (2, 4), (1, 1), (0, 0))),
      ZUNPKD820 -> processZ(Seq((3, 6), (2, 4), (1, 2), (0, 0))),
      ZUNPKD830 -> processZ(Seq((3, 7), (2, 4), (1, 3), (0, 0))),
      ZUNPKD831 -> processZ(Seq((3, 7), (2, 5), (1, 3), (0, 1))),
      ZUNPKD832 -> processZ(Seq((3, 7), (2, 6), (1, 3), (0, 2))),
    ).map(i => i._1 -> i._2(rs1))

  def processS(seq: Seq[(Int, Int)]) = (rs1: UInt) => {
    val out = Seq.fill(4)(Wire(UInt(16.W)))
    for ((x, y) <- seq)
      out(x) := SE16(rs1.B(y))
    (out.reverse.reduce(_ ## _), false.B)
  }

  def processZ(seq: Seq[(Int, Int)]) = (rs1: UInt) => {
    val out = Seq.fill(4)(Wire(UInt(16.W)))
    for ((x, y) <- seq)
      out(x) := ZE16(rs1.B(y))
    (out.reverse.reduce(_ ## _), false.B)
  }

}
