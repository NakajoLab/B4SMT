package b4smt.modules.outputcollector

import b4smt.Parameters
import chiselformal.SymbiYosysFormal
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec

class OutputCollector2Test
    extends AnyFlatSpec
    with ChiselScalatestTester
    with SymbiYosysFormal {
  behavior of "output collector 2"
  implicit val params: b4smt.Parameters =
    Parameters(threads = 4, parallelOutput = 4)
  it should "check formal" in {
    symbiYosysCheck(new OutputCollector2())
  }
}
