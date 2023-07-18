package b4processor.modules.reservationstation

import b4processor.Parameters
import b4processor.utils.Tag
import b4processor.utils.operations.ALUOperation
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor

/** デコーダとリザベーションステーションをつなぐ
  *
  * @param params
  *   パラメータ
  */
class ReservationStationEntry(implicit params: Parameters) extends Bundle {
  val operation = ALUOperation()
  val sourceTag1 = new Tag()
  val ready1 = Bool()
  val value1 = UInt(64.W)
  val sourceTag2 = new Tag()
  val ready2 = Bool()
  val value2 = UInt(64.W)
  val destinationTag = new Tag()
  val wasCompressed = Bool()
  val branchOffset = SInt(12.W)
  val valid = Bool()
}

object ReservationStationEntry {
  def default(implicit params: Parameters): ReservationStationEntry =
    (new ReservationStationEntry).Lit(
      _.operation -> ALUOperation.None,
      _.sourceTag1 -> Tag(0, 0),
      _.ready1 -> false.B,
      _.value1 -> 0.U,
      _.sourceTag2 -> Tag(0, 0),
      _.ready2 -> false.B,
      _.value2 -> 0.U,
      _.destinationTag -> Tag(0, 0),
      _.wasCompressed -> false.B,
      _.valid -> false.B,
      _.branchOffset -> 0.S
    )

  def zero(implicit params: Parameters): ReservationStationEntry =
    (new ReservationStationEntry).Lit(
      _.operation -> ALUOperation.None,
      _.sourceTag1 -> Tag(0, 0),
      _.ready1 -> false.B,
      _.value1 -> 0.U,
      _.sourceTag2 -> Tag(0, 0),
      _.ready2 -> false.B,
      _.value2 -> 0.U,
      _.destinationTag -> Tag(0, 0),
      _.wasCompressed -> false.B,
      _.valid -> false.B,
      _.branchOffset -> 0.S
    )
}
