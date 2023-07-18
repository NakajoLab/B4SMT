package b4processor.utils

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._

class PassthroughBufferTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "pass values" in {
    test(new PassthroughBuffer(UInt(4.W))) { c =>
      c.io.output.ready.poke(true)
      for (i <- 0 until 16) {
        c.clock.step()
        c.io.input.valid.poke(true)
        c.io.input.bits.poke(i)

        c.io.output.valid.expect(true)
        c.io.output.bits.expect(i)
      }
    }
  }

  it should "pass values delayed" in {
    test(new PassthroughBuffer(UInt(4.W))) { c =>
      c.io.output.ready.poke(false)
      c.io.input.valid.poke(true)
      c.io.input.bits.poke(10)

      c.io.output.valid.expect(true)
      c.io.output.bits.expect(10)

      c.clock.step()
      c.io.input.valid.poke(false)
      c.io.input.bits.poke(0)
      c.clock.step(3)

      c.io.output.ready.poke(true)
      c.io.output.valid.expect(true)
      c.io.output.bits.expect(10)

      c.clock.step()

      c.io.output.valid.expect(false)
      c.io.output.bits.expect(0)

    }
  }
}
