package b4smt.utils.operations

import b4smt.Parameters
import b4smt.utils.SymbiYosysFormal
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import chisel3._
import chisel3.util._

class DecodingModTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with SymbiYosysFormal {
  behavior of "Decodign Module"

  implicit val params: b4smt.Parameters = Parameters()

  it should "check formal with pext" in {
    symbiYosysCheck(
      new DecodingMod()(params.copy(enablePExt = true)),
      depth = 3,
    )
  }

  it should "check decoded" in {
    test(new DecodingMod()(params.copy(enablePExt = true))) { c =>
      c.input.poke("b00111000000000000010000001110111".U) // KMDA32
      println(c.out.pextOp.peek())
    }
  }
}
