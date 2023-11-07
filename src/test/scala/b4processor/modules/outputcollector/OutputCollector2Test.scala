package b4processor.modules.outputcollector

import b4processor.Parameters
import b4processor.utils.SymbiYosysFormal
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec

class OutputCollector2Test
    extends AnyFlatSpec
    with ChiselScalatestTester
    with SymbiYosysFormal {
  behavior of "output collector 2"
  implicit val params = Parameters(threads = 4, parallelOutput = 4)
  it should "check formal" in {
    symbiYosysCheck(new OutputCollector2())
  }
}
