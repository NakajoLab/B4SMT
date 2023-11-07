package b4processor.z_B4ProcessorTests

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.utils.B4ProcessorWithMemory
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class z20_B4ProcessorElaborateTest
    extends AnyFlatSpec
    with ChiselScalatestTester {

  // デバッグに時間がかかりすぎるのでパラメータを少し下げる。
  implicit val defaultParams = Parameters(debug = true)

  behavior of "B4Processor elaborate"
  // コンパイルが通ることを確認（信号をつなぎきれていないとエラーになる）
  for (enablePExt <- Seq(false, true))
    for (threads <- Seq(1, 2, 3, 4)) {
      for (pextExecutors <- if (enablePExt) Seq(1, 4) else Seq(0))
        for (executors <- Seq(1, 4))
          for (decoderPerThread <- Seq(1, 3))
            for (maxCommitCount <- Seq(1, 3))
              for (tagWidth <- Seq(2, 5)) {
                it should s"elaborate threads=$threads decoder=$decoderPerThread maxCommitCount=$maxCommitCount tagWidth=$tagWidth executors=$executors pextExe=$pextExecutors pext=$enablePExt" in {
                  ChiselStage.emitCHIRRTL(
                    new B4ProcessorWithMemory()(
                      defaultParams.copy(
                        threads = threads,
                        decoderPerThread = decoderPerThread,
                        maxRegisterFileCommitCount = maxCommitCount,
                        tagWidth = tagWidth,
                        enablePExt = enablePExt,
                        executors = executors,
                        pextExecutors = pextExecutors,
                      ),
                    ),
                  )
                }
              }
    }
}
