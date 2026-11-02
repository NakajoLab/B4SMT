package b4smt.utils

import b4smt.Parameters
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._

class Tag(implicit params: Parameters) extends Bundle {
  val threadId =
    Option.when(params.threads > 1)(UInt(log2Ceil(params.threads).W))
  def getThreadId: UInt = threadId.getOrElse(0.U)
  val id = UInt(params.tagWidth.W)

  override def toPrintable = if (params.threads == 1) {
    cf"Tag(${this.id})"
  } else {
    cf"Tag(thread=${this.getThreadId}, id=${this.id})"
  }

  def ===(that: Tag): Bool =
    this.id === that.id &&
      this.getThreadId === that.getThreadId

  def =/=(other: Tag) = !(this === other)
}

object Tag {
  def apply(threadId: Int, id: Int)(implicit params: Parameters): Tag =
    if (params.threads > 1)
      new Tag().Lit(_.threadId.get -> threadId.U, _.id -> id.U)
    else new Tag().Lit(_.id -> id.U)

  def apply(threadId: UInt, id: UInt)(implicit params: Parameters): Tag = {
    val w = Wire(new Tag)
    w.threadId.foreach(_ := threadId)
    w.id := id
    w
  }
}
