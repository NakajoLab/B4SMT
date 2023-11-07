package b4processor.modules.reservationstation

import b4processor.Parameters
import b4processor.modules.PExt.PExtensionOperation
import b4processor.modules.reorderbuffer.ReorderBufferEntry
import b4processor.utils.RVRegister.AddRegConstructor
import b4processor.utils.{ForPext, Tag, TagValueBundle}
import b4processor.utils.operations.ALUOperation
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor

import scala.math

/** デコーダとリザベーションステーションをつなぐ
  *
  * @param params
  *   パラメータ
  */
class ReservationStationEntry(implicit params: Parameters) extends Bundle {
  val operation = ALUOperation()
  val sources = Vec(3, new TagValueBundle)
  val destinationTag = new Tag()
  val wasCompressed = Bool()
  val branchOffset = SInt(12.W)
  val pextOperation = PExtensionOperation()
  val ispext = Bool()
  val valid = Bool()
}

object ReservationStationEntry {

  def default(implicit params: Parameters) = zero

  def zero(implicit params: Parameters): ReservationStationEntry =
    new ReservationStationEntry().Lit(
      _.sources(0) -> new TagValueBundle().zero,
      _.sources(1) -> new TagValueBundle().zero,
      _.sources(2) -> new TagValueBundle().zero,
      _.operation -> ALUOperation.BranchEqual,
      _.pextOperation -> PExtensionOperation.ADD16,
      _.ispext -> false.B,
      _.destinationTag -> Tag(0, 0),
      _.wasCompressed -> false.B,
      _.branchOffset -> 0.S,
      _.valid -> false.B,
    )
}
