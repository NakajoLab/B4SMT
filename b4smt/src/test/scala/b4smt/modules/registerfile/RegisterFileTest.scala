package b4smt.modules.registerfile

import b4smt.Parameters
import b4smt.utils.RVRegister.{AddRegConstructor, AddUIntRegConstructor}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._

class RegisterFileTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "register file"

  implicit val detfaultParams: b4smt.Parameters =
    Parameters(
      threads = 1,
      decoderPerThread = 1,
      maxRegisterFileCommitCount = 1,
    )

  it should "save a value" in {
    test(new RegisterFile) { c =>
      c.io.reorderBuffer(0).valid.poke(true)
      c.io.reorderBuffer(0).bits.value.poke(123)
      c.io.reorderBuffer(0).bits.destinationRegister.poke(5.reg)

      c.clock.step()

      c.io.decoders(0).sourceRegisters(0).poke(5.reg)
      c.io.decoders(0).values(0).expect(123)
    }
  }

  it should "have no op on 0" in {
    test(new RegisterFile) { c =>
      c.io.reorderBuffer(0).valid.poke(true)
      c.io.reorderBuffer(0).bits.value.poke(123)
      c.io.reorderBuffer(0).bits.destinationRegister.poke(0.reg)

      c.clock.step()

      c.io.decoders(0).sourceRegisters(0).poke(0.reg)
      c.io.decoders(0).values(0).expect(0)
    }
  }

  it should "resolve multiple inputs and outputs" in {
    test(
      new RegisterFile()(detfaultParams.copy(maxRegisterFileCommitCount = 2)),
    ) { c =>
      c.io.reorderBuffer(0).valid.poke(true)
      c.io.reorderBuffer(0).bits.value.poke(123)
      c.io.reorderBuffer(0).bits.destinationRegister.poke(1.reg)
      c.io.reorderBuffer(1).valid.poke(true)
      c.io.reorderBuffer(1).bits.value.poke(456)
      c.io.reorderBuffer(1).bits.destinationRegister.poke(2.reg)

      c.clock.step()

      c.io.decoders(0).sourceRegisters(0).poke(1.reg)
      c.io.decoders(0).values(0).expect(123)
      c.io.decoders(0).sourceRegisters(1).poke(2.reg)
      c.io.decoders(0).values(1).expect(456)
    }
  }

  it should "resolve multiple inputs overlapping" in {
    test(
      new RegisterFile()(detfaultParams.copy(maxRegisterFileCommitCount = 2)),
    ) { c =>
      c.io.reorderBuffer(0).valid.poke(true)
      c.io.reorderBuffer(0).bits.value.poke(123)
      c.io.reorderBuffer(0).bits.destinationRegister.poke(1.reg)
      c.io.reorderBuffer(1).valid.poke(true)
      c.io.reorderBuffer(1).bits.value.poke(456)
      c.io.reorderBuffer(1).bits.destinationRegister.poke(1.reg)

      c.clock.step()

      c.io.decoders(0).sourceRegisters(0).poke(1.reg)
      c.io.decoders(0).values(0).expect(456)
    }
  }
}
