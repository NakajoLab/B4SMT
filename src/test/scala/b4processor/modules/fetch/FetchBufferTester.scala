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
      def inputValue(instruction: UInt, programCounter: UInt, valid: Bool, index: Int): Unit = {
        c.io.input.toBuffer(index).bits.instruction.poke(instruction)
        c.io.input.toBuffer(index).bits.programCounter.poke(programCounter)
        c.io.input.toBuffer(index).valid.poke(valid)
      }
      def initialise(): Unit = {
        for(i <- 0 until defaultParams.decoderPerThread) {
          inputValue(0.U, 0.U, false.B, i)
        }
        readValue(defaultParams.decoderPerThread)
      }
      def readValue(readnum: Int): Unit = {
        for(i <- 0 until readnum) {
          c.io.output(i).ready.poke(true.B)
        }
      }

      initialise()
      inputValue("h00114514".U, "h80000000".U, true.B, 0)
      inputValue("h01919810".U, "h80000004".U, true.B, 1)
      // inputValue("h00114514".U, "h80000004".U, true.B, 1)
      c.clock.step()
      initialise()
      inputValue("h01234567".U, "h80000008".U, true.B, 0)
      c.clock.step()
      initialise()
      inputValue("h00801074".U, "h8000000C".U, true.B, 0)
      c.clock.step()
    }
  }
}
