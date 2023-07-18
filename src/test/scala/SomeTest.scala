import chisel3._
import chiseltest._
import chiseltest.formal.{BoundedCheck, Formal, changed, past}
import chiseltest.simulator.SimulatorDebugAnnotation
import org.scalatest.flatspec._

class Adder extends Module {
  val a = IO(Input(SInt(8.W)))
  val b = IO(Input(SInt(4.W)))
  val out = IO(Output(SInt(8.W)))

  out := a + b
}

class SomeTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "10-3=7" in {
    test(new Adder).withAnnotations(
      Seq(WriteFstAnnotation, VerilatorBackendAnnotation)
    ) { c =>
      c.a.poke(10)
      c.b.poke(-3)
      c.out.expect(7)
      c.clock.step()
    }
  }
}
