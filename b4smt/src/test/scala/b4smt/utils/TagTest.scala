package b4smt.utils

import b4smt.Parameters
import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TagTest extends AnyFlatSpec with ChiselScalatestTester {
  override def autoResetEnabled: Boolean = true

  for (threads <- Seq(1, 2, 3, 4)) {
    it should s"omit or size the thread field for $threads threads" in {
      implicit val params: Parameters = Parameters(threads = threads)
      test(new Module {
        val input = IO(Input(new Tag))
        val output = IO(Output(new Tag))
        val thread = IO(Output(UInt(log2Ceil(threads).max(1).W)))
        require(input.elements.contains("threadId") == (threads > 1))
        require(
          input.threadId
            .map(_.getWidth) == Option.when(threads > 1)(log2Ceil(threads)),
        )
        output := Tag(input.getThreadId, input.id)
        thread := input.getThreadId
      }) { dut =>
        for (tid <- 0 until threads) {
          dut.input.poke(Tag(tid, 1))
          dut.output.expect(Tag(tid, 1))
          dut.thread.expect(tid.U)
        }
      }
    }
  }
}
