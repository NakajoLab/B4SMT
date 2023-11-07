package b4processor.utils

import b4processor.utils.BundleInitialize.AddBundleInitializeConstructor
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.SourceInfo

import scala.language.implicitConversions

class RVRegister extends Bundle {
  override def toPrintable = p"reg(${inner})"

  val inner = UInt(5.W)

  def =/=(other: RVRegister) = this.inner =/= other.inner
}

object RVRegister {
  def apply(num: UInt): RVRegister =
    new RVRegister().initialize(_.inner -> num)
  def apply(num: Int): RVRegister = new RVRegister().Lit(_.inner -> num.U)

  implicit class AddUIntRegConstructor(x: UInt) {
    def reg(implicit sourceInfo: SourceInfo): RVRegister = {
      require(1 <= x.getWidth)
      require(x.getWidth <= 5)
      val w = Wire(new RVRegister())
      w.inner := x
      w
    }
  }

  implicit class AddRegConstructor(x: Int) {
    def reg(implicit sourceInfo: SourceInfo): RVRegister = {
      require(0 <= x)
      require(x <= 31)
      RVRegister(x)
    }
  }
}
