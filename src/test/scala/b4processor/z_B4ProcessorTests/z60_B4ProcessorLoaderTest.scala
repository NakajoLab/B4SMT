package b4processor.z_B4ProcessorTests

import b4processor.Parameters
import b4processor.utils.B4ProcessorWithMemory
import chiseltest._
import chiseltest.internal.CachingAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class z60_B4ProcessorLoaderTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "B4Processor"
  // デバッグに時間がかかりすぎるのでパラメータを少し下げる。
  implicit val defaultParams =
    Parameters(debug = true, tagWidth = 4, threads = 1, decoderPerThread = 1)
  val backendAnnotation = IcarusBackendAnnotation
  val WriteWaveformAnnotation = WriteFstAnnotation

  // branchプログラムが実行できる
  ignore should "execute branch with no parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 1, decoderPerThread = 1)
      )
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation)
      ) { c =>
        c.initialize("programs/riscv-sample-programs/loader-hello")
        c.checkForRegister(13, 20, 100)
      }
  }
}
