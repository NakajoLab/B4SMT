package b4processor.utils

import chisel3._
import b4processor.Parameters

class TagValueBundle(implicit params: Parameters)
    extends EitherBundle(new Tag, UInt(64.W)) {
  def fromTag(tag: Tag) = this.fromLeft(tag)
  def fromValue(value: UInt) = this.fromRight(value)
  def swapToValue(value: UInt) =
    this.matchExhaustive({ l => fromValue(value) }, { r => fromValue(value) })

  def getTagUnsafe = getLeftUnsafe
  def getValueUnsafe = getRightUnsafe
  def isTag = this.isLeft
  def isValue = this.isRight
}
