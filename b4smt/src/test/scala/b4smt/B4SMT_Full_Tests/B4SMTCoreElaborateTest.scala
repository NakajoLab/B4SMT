package b4smt.B4SMT_Full_Tests

import circt.stage.ChiselStage
import b4smt.Parameters
import b4smt.utils.B4SMTCoreWithMemory
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class B4SMTCoreElaborateTest extends AnyFlatSpec with ChiselScalatestTester {

  // デバッグに時間がかかりすぎるのでパラメータを少し下げる。
  implicit val defaultParams: b4smt.Parameters = Parameters(debug = true)

  behavior of "B4SMTCore elaborate"
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
                    new B4SMTCoreWithMemory()(
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
