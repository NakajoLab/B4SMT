package b4processor.utils

import b4processor.utils.BundleInitialize.AddBundleInitializeConstructor
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.SourceInfo

import scala.language.implicitConversions

class VectorRegister extends Bundle {
  val inner = UInt(5.W)

  def =/=(other: VectorRegister): Bool = {
    this.inner =/= other.inner
  }
}

object VectorRegister {
  def apply(num: UInt): VectorRegister = {
    new VectorRegister().initialize(_.inner -> num)
  }
  def apply(num: Int): VectorRegister = VectorRegister(num.U)

  implicit class AddUIntRegConstructor(x: UInt) {
    def reg(implicit sourceInfo: SourceInfo): VectorRegister = {
      require(1 <= x.getWidth)
      require(x.getWidth <= 5)
      val w = Wire(new VectorRegister())
      w.inner := x
      w
    }
  }

  implicit class AddRegConstructor(x: Int) {
    def reg(implicit sourceInfo: SourceInfo): VectorRegister = {
      require(0 <= x)
      require(x <= 31)
      VectorRegister(x)
    }
  }
}