package b4smt.B4SMT_Full_Tests

import b4smt.Parameters
import b4smt.simulator.StandaloneTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.tagobjects.Slow

class B4SMTCoreBenchmark extends AnyFlatSpec with StandaloneTests {

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

  behavior of s"RISC-V benchmark"

  ignore should "run dhrystore" taggedAs Slow in {
    standalone(defaultParams) { c =>
      c.initialize(
        "programs/riscv-tests/share/riscv-tests/benchmarks/dhrystone",
      )

      c.monitorWrites(1000000, 0x80001000L)
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
      standalone(
        defaultParams.copy(
          threads = 4,
          enablePExt = true,
          tagWidth = 5,
          decoderPerThread = 2,
        ),
      ) { c =>
        c.initialize(
          s"programs/riscv-tests/share/riscv-tests/benchmarks/$filename",
        )
        c.outputReady(true)
        c.outputUntilSuffix("end", 100000, 1000000, print = true)
      }
    }
  }
}
