package b4processor.modules.fetch

import b4processor._
import chiseltest._
import org.scalatest.flatspec._
import chisel3._
import chisel3.util._

class FetchBufferTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FetchBuffer"
  implicit val defaultParams =
    Parameters(
      debug = true,
      threads = 1,
      decoderPerThread = 2,
      instructionStart = 0x1000_0000
    )

  it should "not act sussy" in {
    test(new FetchBuffer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // TODO: write some tests to fuck
      def inputValue(instruction: UInt, programCounter: UInt, index: Int): Unit = {
        c.io.input.toBuffer(index).bits.instruction.poke(instruction)
        c.io.input.toBuffer(index).bits.programCounter.poke(programCounter)
        c.io.input.toBuffer(index).valid.poke(true.B)
      }
    }
  }
}
