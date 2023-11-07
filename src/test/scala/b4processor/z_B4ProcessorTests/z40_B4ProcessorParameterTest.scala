package b4processor.z_B4ProcessorTests

import b4processor.Parameters
import b4processor.utils.B4ProcessorWithMemory
import chiseltest._
import chiseltest.internal.CachingAnnotation
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.tagobjects.Slow

import java.io.FileWriter

object ParameterTest extends Tag("ParameterTest")

class z40_B4ProcessorParameterTest
    extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "B4Processor with many parameters"
  implicit val defaultParams = Parameters(debug = true)

  for (threads <- Seq(1, 2, 5, 6, 7, 8)) {
    for (executors <- Seq(1, 8))
      for (decoderPerThread <- Seq(4))
        for (maxCommitCount <- Seq(2))
          for (tagWidth <- Seq(6))
            for (lsqWidth <- Seq(4)) {
              val title =
                s"run fibonacci_c threads=$threads executor=$executors decoders=$decoderPerThread maxCommitCount=$maxCommitCount tagWidth=$tagWidth lsqWidth=$lsqWidth"
              it should title taggedAs (ParameterTest, Slow) in {
                test(
                  new B4ProcessorWithMemory()(
                    defaultParams.copy(
                      threads = threads,
                      executors = executors,
                      decoderPerThread = decoderPerThread,
                      tagWidth = tagWidth,
                      maxRegisterFileCommitCount = maxCommitCount,
                      loadStoreQueueIndexWidth = lsqWidth,
                    ),
                  ),
                )
                  .withAnnotations(
                    Seq(
                      WriteFstAnnotation,
                      VerilatorBackendAnnotation,
                      CachingAnnotation,
                    ),
                  ) { c =>
                    c.initialize64("programs/riscv-sample-programs/fibonacci_c")
                    for (t <- 0 until threads)
                      c.checkForRegisterChange(
                        3,
                        1298777728820984005L,
                        20000,
                        t,
                      )
                    val fw = new FileWriter("stats.jsonl", true)
                    val ipcs = (0 until threads)
                      .map(t =>
                        c.io.registerFileContents
                          .get(t)(6)
                          .peekInt()
                          .toDouble / c.io.registerFileContents
                          .get(t)(5)
                          .peekInt()
                          .toDouble,
                      )
                      .map(_.toString)
                      .reduce((a, b) => a + "," + b)
                    try {
                      fw.write(
                        s"{\"threads\":$threads, \"executor\":$executors, \"decoders\":$decoderPerThread, \"maxCommitCount\":$maxCommitCount, \"tagWidth\":$tagWidth, \"lsqWidth\":${lsqWidth}, \"ipc\":[$ipcs]}\n",
                      )
                    } finally fw.close()

                  }
              }
            }
  }
}
