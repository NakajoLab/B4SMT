package b4smt.B4SMT_Full_Tests

import b4smt.Parameters
import b4smt.utils.B4SMTCoreWithMemory
import chiseltest._
import chiseltest.internal.CachingAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.tagobjects.Slow

class B4SMTCoreBenchmark extends AnyFlatSpec with ChiselScalatestTester {
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
  val backendAnnotation = IcarusBackendAnnotation
  val WriteWaveformAnnotation = WriteFstAnnotation

  behavior of s"RISC-V benchmark"

  ignore should "run dhrystore" taggedAs Slow in {
    test(new B4SMTCoreWithMemory).withAnnotations(
      Seq(
        WriteWaveformAnnotation,
        CachingAnnotation,
        VerilatorBackendAnnotation,
      ),
    ) { c =>
      c.initialize(
        "programs/riscv-tests/share/riscv-tests/benchmarks/dhrystone",
      )
      c.clock.setTimeout(1000000)
      fork(while (true) {
        if (
          c.io.accessMemoryAddress.writeAddress.valid.peekBoolean() &&
          c.io.accessMemoryAddress.writeAddress.bits.peekInt() == 0x80001000L
        ) {
          while (!c.io.accessMemoryAddress.writeData.valid.peekBoolean()) {
            c.clock.step()
          }
          val ch = c.io.accessMemoryAddress.writeData.bits.peekInt()
          print(ch.toChar)
        }
        c.clock.step()
      })
      c.clock.step(1000000)
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
      test(
        new B4SMTCoreWithMemory()(
          defaultParams.copy(
            threads = 4,
            enablePExt = true,
            tagWidth = 5,
            decoderPerThread = 2,
          ),
        ),
      )
        .withAnnotations(
          Seq(
            WriteWaveformAnnotation,
            CachingAnnotation,
            VerilatorBackendAnnotation,
          ),
        ) { c =>
          c.initialize(
            s"programs/riscv-tests/share/riscv-tests/benchmarks/$filename",
          )
          c.io.simulationIO.output.ready.poke(true)
          var inputs = Seq(' ', ' ', ' ');
          while (inputs != Seq('e', 'n', 'd')) {
            val p = c.getOutput(100000, print_value = true)
            inputs = Seq(inputs(1), inputs(2), p)
          }
        }
    }
  }
}
