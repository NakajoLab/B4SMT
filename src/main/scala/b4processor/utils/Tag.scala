package b4processor.utils

import b4processor.Parameters
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._

class Tag(implicit params: Parameters) extends Bundle {
  val threadId = UInt(log2Up(params.threads).W)
  val id = UInt(params.tagWidth.W)

  override def toPrintable = if (params.threads == 1) {
    cf"Tag(${this.id})"
  } else {
    cf"Tag(thread=${this.threadId}, id=${this.id})"
  }

  def ===(that: Tag): Bool =
    this.id === that.id &&
      this.threadId === that.threadId

  def =/=(other: Tag) = !(this === other)
}

object Tag {
  def apply(threadId: Int, id: Int)(implicit params: Parameters): Tag =
    new Tag().Lit(_.threadId -> threadId.U, _.id -> id.U)

  def apply(threadId: UInt, id: UInt)(implicit params: Parameters): Tag = {
    val w = Wire(new Tag)
    w.threadId := threadId
    w.id := id
    w
  }
}
