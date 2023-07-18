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
  for (threads <- Seq(1, 2, 3, 4, 5, 6, 7, 8))
    for (decoderPerThread <- 1 to 3)
      for (maxCommitCount <- 1 to 3)
        for (tagWidth <- 2 to 3)
          it should s"elaborate threads=$threads decoder=$decoderPerThread maxCommitCount=$maxCommitCount tagWidth=$tagWidth" in {
            ChiselStage.emitCHIRRTL(
              new B4ProcessorWithMemory()(
                defaultParams.copy(
                  threads = threads,
                  decoderPerThread = decoderPerThread,
                  maxRegisterFileCommitCount = maxCommitCount,
                  tagWidth = tagWidth
                )
              )
            )
          }
}
