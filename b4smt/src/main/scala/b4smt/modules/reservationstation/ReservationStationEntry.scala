package b4smt.modules.reservationstation

import b4smt.Parameters
import b4smt.modules.reorderbuffer.ReorderBufferEntry
import b4smt.utils.RVRegister.AddRegConstructor
import b4smt.utils.{ForPext, Tag, TagValueBundle}
import b4smt.utils.operations.{ALUOperation, MulDivOperation}
import b4smt_pext.PExtensionOperation
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor

import scala.math

object ExecutorType extends ChiselEnum {
  val Regular, MulDiv, PExt = Value
}

/** デコーダとリザベーションステーションをつなぐ
  *
  * @param params
  *   パラメータ
  */
class ReservationStationEntry(implicit params: Parameters) extends Bundle {
  val operation = ALUOperation()
  val pextOperation = PExtensionOperation()
  val mulDivOperation = MulDivOperation()
  val exeType = ExecutorType.Type()

  val sources = Vec(3, new TagValueBundle)
  val destinationTag = new Tag()
  val wasCompressed = Bool()
  val branchOffset = SInt(12.W)
  val valid = Bool()
}

object ReservationStationEntry {

  def default(implicit params: Parameters) = zero

  def zero(implicit params: Parameters): ReservationStationEntry =
    new ReservationStationEntry().Lit(
      _.sources(0) -> TagValueBundle.zero,
      _.sources(1) -> TagValueBundle.zero,
      _.sources(2) -> TagValueBundle.zero,
      _.operation -> ALUOperation.BranchEqual,
      _.pextOperation -> PExtensionOperation.ADD16,
      _.mulDivOperation -> MulDivOperation.Mul,
      _.exeType -> ExecutorType.Regular,
      _.destinationTag -> Tag(0, 0),
      _.wasCompressed -> false.B,
      _.branchOffset -> 0.S,
      _.valid -> false.B,
    )
}
