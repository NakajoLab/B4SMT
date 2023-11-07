package b4processor.utils

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._

class EitherBundle[L <: Data, R <: Data](ltype: L, rtype: R) extends Bundle {
  private val common_width = ltype.getWidth max rtype.getWidth
  val __internal_common_data = UInt(common_width.W)
  val __internal_is_left = Bool()

  def fromConditional(
    isLeftCond: Bool,
    left: L,
    right: R,
  ): EitherBundle[L, R] = {
    val w = Wire(this)
    w.__internal_is_left := isLeftCond
    w.__internal_common_data := Mux(
      isLeftCond,
      left.asTypeOf(__internal_common_data),
      right.asTypeOf(__internal_common_data),
    )
    w
  }

  def fromLeft(v: L): EitherBundle[L, R] = {
    val w = Wire(this.cloneType)
    w.__internal_is_left := true.B
    w.__internal_common_data := v.asTypeOf(this.__internal_common_data)
    w
  }

  def fromRight(v: R): EitherBundle[L, R] = {
    val w = Wire(this.cloneType)
    w.__internal_is_left := false.B
    w.__internal_common_data := v.asTypeOf(this.__internal_common_data)
    w
  }

  def getLeftUnsafe =
    this.__internal_common_data(ltype.getWidth - 1, 0).asTypeOf(ltype)
  def getRightUnsafe =
    this.__internal_common_data(rtype.getWidth - 1, 0).asTypeOf(rtype)

  def isLeft = this.__internal_is_left
  def isRight = !this.isLeft
  def zero =
    this.Lit(_.__internal_is_left -> false.B, _.__internal_common_data -> 0.U)

  def matchExhaustive[T <: Data](left: L => T, right: R => T) = {
    EitherBundle.matchExhaustive(this, left, right)
  }

  def transpose(data: EitherBundle[L, R]): EitherBundle[R, L] = {
    this.matchExhaustive(
      { (l: L) => new EitherBundle(rtype, ltype).fromRight(l) },
      { (r: R) => new EitherBundle(rtype, ltype).fromLeft(r) },
    )
  }
}

object EitherBundle {

  def matchExhaustive[L <: Data, R <: Data, T <: Data](
    data: EitherBundle[L, R],
    left: L => T,
    right: R => T,
  ): T = {
    val left_val = left(data.getLeftUnsafe)
    val right_val = right(data.getRightUnsafe)
    Mux(data.isLeft, left_val, right_val)
  }

}
