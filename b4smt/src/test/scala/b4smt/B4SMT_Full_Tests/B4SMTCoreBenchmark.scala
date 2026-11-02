package b4smt.B4SMT_Full_Tests

import b4smt.Parameters
import b4smt.utils.B4SMTCoreWithMemory
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.tagobjects.Slow

class B4SMTCoreBenchmark extends AnyFlatSpec with ChiselSim {
  override def configMap = super.configMap + ("emitVcd" -> "1")

  // デバッグに時間がかかりすぎるのでパラメータを少し下げる。
  implicit val defaultParams: b4smt.Parameters = {
    Parameters(
      debug = true,
      threads = 1,
      decoderPerThread = 1,
      tagWidth = 5,
      loadStoreQueueIndexWidth = 2,
      maxRegisterFileCommitCount = 2,
      instructionStart = 0x8000_0000L,
    )
  }

  // Preserve waveform tracing through ChiselSim's native emitVcd configuration.

  behavior of s"RISC-V benchmark"

  ignore should "run dhrystore" taggedAs Slow in {
    simulate(new B4SMTCoreWithMemory) { c =>
      c.initialize(
        "programs/riscv-tests/share/riscv-tests/benchmarks/dhrystone",
      )

      // Sample the monitor before each deterministic clock step.
      var waitingForData = false
      for (_ <- 0 until 1000000) {
        if (
          !waitingForData &&
          c.io.accessMemoryAddress.writeAddress.valid.peekBoolean() &&
          c.io.accessMemoryAddress.writeAddress.bits
            .peek()
            .litValue == 0x80001000L
        ) {
          waitingForData = true
        }
        if (
          waitingForData && c.io.accessMemoryAddress.writeData.valid
            .peekBoolean()
        ) {
          print(c.io.accessMemoryAddress.writeData.bits.peek().litValue.toChar)
          waitingForData = false
        }
        c.clock.step()
      }
    }
  }

  for (
    filename <- Seq(
      "median",
      "median-mt",
      "median-p",
      "median-p-mt",
      "median-p-byte",
      "median-p-mt-byte",
    )
  ) {
    ignore should s"run $filename" taggedAs Slow in {
      simulate(
        new B4SMTCoreWithMemory()(
          defaultParams.copy(
            threads = 4,
            enablePExt = true,
            tagWidth = 5,
            decoderPerThread = 2,
          ),
        ),
      ) { c =>
        c.initialize(
          s"programs/riscv-tests/share/riscv-tests/benchmarks/$filename",
        )
        c.io.simulationIO.output.ready.poke(true)
        var inputs = Seq(' ', ' ', ' ');
        var characters = 0
        while (inputs != Seq('e', 'n', 'd') && characters < 1000000) {
          characters += 1
          val p = c.getOutput(100000, print_value = true)
          inputs = Seq(inputs(1), inputs(2), p)
        }
        assert(
          inputs == Seq('e', 'n', 'd'),
          "benchmark did not emit end within 1000000 characters",
        )
      }
    }
  }
}
