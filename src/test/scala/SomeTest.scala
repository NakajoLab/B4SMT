import b4processor.utils.SymbiYosysFormal
import chisel3._
import chiseltest._
import chiseltest.formal.{BoundedCheck, Formal, changed, past}
import chiseltest.simulator.SimulatorDebugAnnotation
import circt.stage.ChiselStage
import org.scalatest.flatspec._

class Adder extends Module {
  val a = IO(Input(SInt(8.W)))
  val b = IO(Input(SInt(4.W)))
  val out = IO(Output(SInt(8.W)))

  out := a + b

  cover(out === 3.S)
  when(a === 0.S) {
    assert(out === b)
  }
}

object Adder extends App {
  ChiselStage.emitSystemVerilogFile(new Adder)
}

class SomeTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with SymbiYosysFormal {

  behavior of "some test"

  it should "10-3=7" in {
    test(new Adder).withAnnotations(
      Seq(WriteFstAnnotation, VerilatorBackendAnnotation),
    ) { c =>
      c.a.poke(10)
      c.b.poke(-3)
      c.out.expect(7)
      c.clock.step()
    }
  }

  it should "check formal" in {
    symbiYosysCheck(new Adder)
  }
}

class ParameterAdder(n: Int, width: Int) extends Module {
  val inputs = IO(Input(Vec(n, UInt(width.W))))
  val output = IO(Output(UInt(width.W)))

  output := inputs reduce (_ + _)
}

object ParameterAdder extends App {
//  ChiselStage.emitSystemVerilogFile(new ParameterAdder(2, 4))
  ChiselStage.emitSystemVerilogFile(new ParameterAdder(4, 32))
}
