package b4processor.modules.fetch

import b4processor._
import chiseltest._
import org.scalatest.flatspec._

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
    }
  }
}
