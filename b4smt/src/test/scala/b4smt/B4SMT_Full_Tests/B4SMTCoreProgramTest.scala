package b4smt.B4SMT_Full_Tests

import b4smt.Parameters
import b4smt.simulator.StandaloneTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.tagobjects.Slow

class B4SMTCoreProgramTest extends AnyFlatSpec with StandaloneTests {

  behavior of "B4SMT test programs"
  // Reduce the parameters slightly because debugging takes too long.
  implicit val defaultParams: b4smt.Parameters =
    Parameters(
      debug = true,
      tagWidth = 4,
      threads = 1,
      decoderPerThread = 1,
//      enablePExt = true
    )

  // Execute the branch program
  it should "execute branch with no parallel" in {
    standalone(defaultParams.copy(threads = 1, decoderPerThread = 1)) { c =>
      c.initialize("programs/riscv-sample-programs/branch")
      c.checkForRegister(13, 20, 200)
    }
  }
  // Execute the branch program
  it should "execute branch with 2 parallel thread" in {
    standalone(defaultParams.copy(threads = 2, decoderPerThread = 1)) { c =>
      c.initialize("programs/riscv-sample-programs/branch")
      c.checkForRegister(13, 20, 200, 0)
      c.checkForRegister(13, 20, 200, 1)
    }
  }

  // Calculate the Fibonacci sequence with an issue width of 1
  it should "execute fibonacci with no parallel" in {
    standalone(defaultParams.copy(threads = 1, decoderPerThread = 1)) { c =>
      c.initialize("programs/riscv-sample-programs/fibonacci")
      c.checkForRegister(6, 55, 400)
    }
  }

  // Calculate the Fibonacci sequence with an issue width of 2
  it should "execute fibonacci with 2 parallel" in {
    standalone(defaultParams) { c =>
      c.initialize("programs/riscv-sample-programs/fibonacci")
      c.checkForRegister(6, 55, 400)
    }
  }

  // Calculate the Fibonacci sequence with an issue width of 4
  it should "execute fibonacci with 4 parallel" in {
    standalone(defaultParams.copy(threads = 1, decoderPerThread = 4)) { c =>
      c.initialize("programs/riscv-sample-programs/fibonacci")
      c.checkForRegister(6, 55, 200)
    }
  }

  // Correctly execute call (function call using JALR) and ret (return from a function using JALR)
  it should "execute call_ret with 2 parallel" in {
    standalone(defaultParams) { c =>
      c.initialize("programs/riscv-sample-programs/call_ret")
      c.checkForRegister(5, 1, 100)
      c.checkForRegister(6, 2, 20)
      c.checkForRegister(7, 3, 20)
    }
  }

  // Test many potentially parallelizable add instructions with an issue width of 1
  it should "execute many_add with no parallel" in {
    standalone(defaultParams.copy(threads = 1, decoderPerThread = 1)) { c =>
      c.initialize("programs/riscv-sample-programs/many_add")
      c.checkForRegister(1, 8, 100)
    }
  }

  // Test many potentially parallelizable add instructions with an issue width of 1
  it should "execute many_add with no parallel 2 thread" in {
    standalone(defaultParams.copy(threads = 2, decoderPerThread = 1)) { c =>
      c.initialize("programs/riscv-sample-programs/many_add")
      c.checkForRegister(1, 8, 200, 0)
      c.checkForRegister(1, 8, 70, 1)
    }
  }

  // Test many potentially parallelizable add instructions with an issue width of 2
  it should "execute many_add with 2 parallel" in {
    standalone(defaultParams.copy(fetchWidth = 8)) { c =>
      c.initialize("programs/riscv-sample-programs/many_add")
      c.checkForRegister(1, 8, 100)
    }
  }

  // Test many potentially parallelizable add instructions with an issue width of 4
  it should "execute many_add with 4 parallel" in {
    standalone(
      defaultParams.copy(threads = 1, decoderPerThread = 4, fetchWidth = 8),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/many_add")
      c.checkForRegister(1, 8, 70)
    }
  }

  // Work correctly even with a very small tag width (not all decoders can be used)
  it should "execute many_add with 4 parallel with very low tag width" in {
    standalone(
      defaultParams
        .copy(threads = 1, decoderPerThread = 4, fetchWidth = 8, tagWidth = 2),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/many_add")
      c.checkForRegister(1, 8, 70)
    }
  }

  // Test many potentially parallelizable add instructions with an issue width of 8
  it should "execute many_add with 8 parallel" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 8,
        fetchWidth = 8,
        executors = 4,
        maxRegisterFileCommitCount = 10,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/many_add")
      c.checkForRegister(1, 8, 70)
    }
  }

  // Test potentially out-of-order instructions with an issue width of 4
  it should "execute out_of_order with 4 parallel" in {
    standalone(
      defaultParams
        .copy(
          threads = 1,
          decoderPerThread = 4,
          fetchWidth = 8,
          maxRegisterFileCommitCount = 8,
        ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/many_add_out_of_order")
      c.step(60)
      c.expectRegister(1, 1, 0)
      c.expectRegister(2, 2, 0)
      c.expectRegister(3, 3, 0)
      c.expectRegister(4, 4, 0)
      c.expectRegister(5, 1, 0)
      c.expectRegister(6, 2, 0)
      c.expectRegister(7, 3, 0)
      c.expectRegister(8, 4, 0)
      c.expectRegister(9, 1, 0)
      c.expectRegister(10, 2, 0)
      c.expectRegister(11, 3, 0)
      c.expectRegister(12, 4, 0)
    }
  }

  // Program that stores and loads a simple value, with an issue width of 2
  it should "run load_store with 2 parallel" in {
    standalone(defaultParams.copy(threads = 1, decoderPerThread = 2)) { c =>
      c.initialize("programs/riscv-sample-programs/load_store")
      c.checkForRegister(3, 10, 200)
      // address could change
      // c.io.registerFileContents
      //          .get(0)(1)
      //          .expect(defaultParams.instructionStart + 0x58 + 16)
      //          .expect(defaultParams.instructionStart + 0x58 + 16)
      c.expectRegister(2, 10, 0)
      c.expectRegister(3, 10, 0)
    }
  }

  it should "run fibonacci_c" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 1,
        maxRegisterFileCommitCount = 1,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/fibonacci_c")
      c.step(500)
      c.checkForRegisterChange(3, 1298777728820984005L, 10000)
    }
  }

  it should "run fibonacci_c with 2 parallel" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 2,
        maxRegisterFileCommitCount = 4,
        loadStoreQueueIndexWidth = 3,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/fibonacci_c")
      c.checkForRegisterChange(3, 1298777728820984005L, 10000)
    }
  }

  it should "run load_plus_arithmetic" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 4,
        maxRegisterFileCommitCount = 4,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/load_plus_arithmetic")
      c.checkForRegister(2, 20, 50)
      c.checkForRegister(3, 1, 50)
    }
  }

  it should "run load_after_store" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 4,
        maxRegisterFileCommitCount = 4,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/load_after_store")
      c.checkForRegister(3, 10, 100)
    }
  }

  it should "run enter_c" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 4,
        maxRegisterFileCommitCount = 4,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/enter_c")
      c.checkForRegister(3, 5, 100)

    }
  }

  it should "run calculation_c" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 4,
        maxRegisterFileCommitCount = 4,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/calculation_c")
      c.checkForRegister(3, 18, 400)

    }
  }

  it should "run loop_c" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 1,
        maxRegisterFileCommitCount = 1,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/loop_c")
      c.checkForRegister(3, 30, 1000)

    }
  }

  it should "run loop_c with 4 parallel" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 4,
        maxRegisterFileCommitCount = 4,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/loop_c")
      c.checkForRegister(3, 30, 2000)

    }
  }

  it should "run many_load_store" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 1,
        maxRegisterFileCommitCount = 1,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/many_load_store")
      c.checkForRegister(2, 36)

    }
  }

  it should "run many_load_store with 4 parallel" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 4,
        maxRegisterFileCommitCount = 4,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/many_load_store")
      c.checkForRegister(2, 36, 100)

    }
  }

  it should "run load_store_cross" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 1,
        maxRegisterFileCommitCount = 1,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/load_store_cross")
      c.checkForRegister(2, 101, 200)
      c.checkForRegister(3, 201, 200)
    }
  }

  it should "run load_store_cross with 4 parallel" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 4,
        maxRegisterFileCommitCount = 4,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/load_store_cross")
      c.checkForRegister(2, 101, 200)
      c.checkForRegister(3, 201, 200)
    }
  }

  it should "run csrtest" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 1,
        maxRegisterFileCommitCount = 2,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/csrtest")
      c.checkForRegister(17, 10)
      c.checkForRegister(17, 20)
      c.checkForRegister(17, 30)
      c.step(20)
    }
  }

  it should "run illegal_inst" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 1,
        maxRegisterFileCommitCount = 2,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/illegal_inst")
      c.checkForRegister(10, 10)
      c.checkForRegister(10, 40)
      c.step(20)
    }
  }

  it should "run io_test" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 1,
        maxRegisterFileCommitCount = 2,
        loadStoreQueueIndexWidth = 2,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/io_test")
      for (p <- "ABCDEFGHIJK")
        c.checkForOutput(p)
      c.checkForOutput('O')
    }
  }

  it should "run bench" in {
    val p = defaultParams.copy(
      threads = 1,
      decoderPerThread = 2,
      maxRegisterFileCommitCount = 2,
      loadStoreQueueIndexWidth = 3,
      tagWidth = 5,
      executors = 4,
      instructionStart = 0x8000_0000L,
    )
    standalone(p) { c =>
      c.initialize("programs/riscv-sample-programs/bench")
      c.outputReady(true)
      for (_ <- 0 until p.threads)
        c.checkForOutputAny(2000, print_value = true)
      for (_ <- "OK!\n")
        c.checkForOutputAny(2000, print_value = true)
      for (p <- "took ")
        c.checkForOutput(p, 20000, print_value = true)
      c.outputUntilSuffix(" ", 2000, 32, print = true)
      for (p <- "cycles\n")
        c.checkForOutput(p, 1000, print_value = true)

      for (n <- Seq(1, 2, 3, 5, 8, 13, 21, 34, 55))
        for (p <- n.toString + "\n")
          c.checkForOutput(p, 2000, print_value = true)
    }
  }

  it should "run pext_test" in {
    standalone(
      defaultParams.copy(
        threads = 1,
        decoderPerThread = 1,
        maxRegisterFileCommitCount = 1,
        loadStoreQueueIndexWidth = 2,
        enablePExt = true,
      ),
    ) { c =>
      c.initialize("programs/riscv-sample-programs/pext_test")
      c.step(300)
    }
  }

  it should "run testhex" taggedAs Slow in {
    standalone(
      defaultParams.copy(
        threads = 16,
        tagWidth = 6,
        reservationStationWidth = 5,
        decoderPerThread = 2,
        executors = 8,
        maxRegisterFileCommitCount = 2,
        loadStoreQueueIndexWidth = 4,
        enablePExt = true,
        pextExecutors = 1,
        parallelOutput = 1,
      ),
    ) { c =>
      // This long-running test intentionally does not dump waveforms.
      c.initialize("test")
      c.outputReady(true)
      c.checkForOutputString("START", 10000, print_value = true)
      c.checkForOutputString("END", 100000000, print_value = true)
    }
  }
}
