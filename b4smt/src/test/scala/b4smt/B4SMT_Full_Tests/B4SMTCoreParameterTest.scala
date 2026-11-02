package b4smt.B4SMT_Full_Tests

import b4smt.Parameters
import b4smt.simulator.StandaloneTests
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.tagobjects.Slow

object ParameterTest extends Tag("ParameterTest")

class z40_B4SMTCoreParameterTest extends AnyFlatSpec with StandaloneTests {

  behavior of "B4SMT with many parameters"
  implicit val defaultParams: b4smt.Parameters = Parameters(debug = true)

  for (threads <- Seq(1, 2, 5, 6, 7, 8)) {
    for (executors <- Seq(1, 8))
      for (decoderPerThread <- Seq(4))
        for (maxCommitCount <- Seq(2))
          for (tagWidth <- Seq(6))
            for (lsqWidth <- Seq(4)) {
              val title =
                s"run fibonacci_c threads=$threads executor=$executors decoders=$decoderPerThread maxCommitCount=$maxCommitCount tagWidth=$tagWidth lsqWidth=$lsqWidth"
              it should title taggedAs (ParameterTest, Slow) in {
                standalone(
                  defaultParams.copy(
                    threads = threads,
                    executors = executors,
                    decoderPerThread = decoderPerThread,
                    tagWidth = tagWidth,
                    maxRegisterFileCommitCount = maxCommitCount,
                    loadStoreQueueIndexWidth = lsqWidth,
                  ),
                ) { c =>
                  c.initialize64("programs/riscv-sample-programs/fibonacci_c")
                  for (t <- 0 until threads)
                    c.checkForRegisterChange(3, 1298777728820984005L, 20000, t)
                  c.statistics(
                    "stats.jsonl",
                    ujson.Obj(
                      "threads" -> threads,
                      "executor" -> executors,
                      "decoders" -> decoderPerThread,
                      "maxCommitCount" -> maxCommitCount,
                      "tagWidth" -> tagWidth,
                      "lsqWidth" -> lsqWidth,
                    ),
                  )

                }
              }
            }
  }
}
