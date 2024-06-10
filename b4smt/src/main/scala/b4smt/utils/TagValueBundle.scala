package b4smt.utils

import b4smt.Parameters
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor

class TagValueBundle(implicit params: Parameters) extends Bundle {

  /** bundle represents tag */
  val isTag = Bool()

  /** value data */
  val value = UInt(64.W)

  /** tag data */
  val tag = new Tag()

  /** bundle represents value. read only. */
  def isValue = !isTag

  def matchExhaustive(
    tagFn: Tag => TagValueBundle,
    valueFn: UInt => TagValueBundle,
  )(implicit params: Parameters): TagValueBundle = {
    val w = Wire(new TagValueBundle)
    w := DontCare
    w.isTag := isTag
    w.tag := tag
    w.value := value
    w
  }
}

object TagValueBundle {
  def fromTag(tag: Tag)(implicit params: Parameters): TagValueBundle = {
    val w = Wire(new TagValueBundle)
    w := DontCare
    w.isTag := true.B
    w.tag := tag
    w
  }

  def fromValue(value: UInt)(implicit params: Parameters): TagValueBundle = {
    require(value.getWidth == 64)
    val w = Wire(new TagValueBundle)
    w := DontCare
    w.isTag := false.B
    w.value := value
    w
  }

  def fromConditional(isTag: Bool, tag: Tag, value: UInt)(implicit
    params: Parameters,
  ): TagValueBundle = {
    val w = Wire(new TagValueBundle)
    w.isTag := isTag
    w.tag := tag
    w.value := value
    w
  }

  def zero(implicit params: Parameters): TagValueBundle = (new TagValueBundle())
    .Lit(_.tag -> Tag(0, 0), _.value -> 0.U, _.isTag -> false.B)
}
