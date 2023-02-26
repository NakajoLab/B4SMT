package b4processor.modules.csr

import b4processor.Parameters
import b4processor.utils.Tag
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._

class CSRWrapper(implicit params: Parameters) extends CSR {
  def setDecoderInput(
    address: UInt = 0.U,
    destinationTag: Tag = Tag(0, 0),
    value: UInt = 0.U
  ): Unit = {
    this.io.decoderInput.valid.poke(true)
    this.io.decoderInput.bits.address.poke(address)
    this.io.decoderInput.bits.value.poke(value)
    this.io.decoderInput.bits.destinationTag.poke(destinationTag)
  }

  def setThreadId(threadId: Int): Unit = {
    this.io.threadId.poke(threadId)
  }

  def setReorderBuffer(retireCount: Int = 0): Unit = {
    this.io.reorderBuffer.retireCount.poke(retireCount)
  }

  def expectOutput(value: UInt = 0.U, isError: Boolean = false): Unit = {
    this.io.CSROutput.bits.value.expect(value)
    this.io.CSROutput.bits.isError.expect(isError)
  }
}

class CSRTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CSR"

  implicit val params = Parameters()

  it should "return clock cycles" in {
    test(new CSRWrapper) { c =>
      c.clock.step(100)
      c.setDecoderInput(CSRName.cycle)
      c.expectOutput(100.U)
    }
  }

  it should "return retire count" in {
    test(new CSRWrapper()(params.copy(maxRegisterFileCommitCount = 10))) { c =>
      c.clock.step()
      c.setReorderBuffer(1)
      c.clock.step()
      c.setReorderBuffer(2)
      c.clock.step()
      c.setReorderBuffer()
      c.clock.step()

      c.setDecoderInput(CSRName.instret)
      c.expectOutput(3.U)
    }
  }

  it should "return mhartid" in {
    test(new CSRWrapper()(params.copy(threads = 10))) { c =>
      c.setDecoderInput(CSRName.mhartid)
      c.setThreadId(5)
      c.expectOutput(5.U)
    }
  }

  it should "error on time" in {
    test(new CSRWrapper) { c =>
      c.setDecoderInput(CSRName.time)
      c.expectOutput(isError = true)
    }
  }
}
