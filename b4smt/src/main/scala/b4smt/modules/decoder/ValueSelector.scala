package b4smt.modules.decoder

import b4smt.Parameters
import b4smt.connections.CollectedOutput
import b4smt.utils.Tag
import chisel3._
import chisel3.util._

/** Circuit that selects the value of source tag 1
  *
  * @param params
  *   Parameters
  */
class ValueSelector(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val reorderBufferValue = Flipped(Valid(UInt(64.W)))
    val registerFileValue = Input(UInt(64.W))
    val outputCollector = Flipped(new CollectedOutput)
    val sourceTag = Input(Valid(new Tag))
    val value = Valid(UInt(64.W))
  })

  // Among the values bypassed from the ALU, find one whose destination tag matches the source tag
  private val outputMatchingTagExists = io.outputCollector.outputs
    .map(o => o.valid && o.bits.tag === io.sourceTag.bits)
    .fold(false.B)(_ || _)

  // Whether a value is available
  io.value.valid := MuxCase(
    false.B,
    Seq(
      (io.sourceTag.valid && io.reorderBufferValue.valid) -> true.B,
      (io.sourceTag.valid && outputMatchingTagExists) -> true.B,
      (!io.sourceTag.valid) -> true.B,
    ),
  )
  // Value contents
  io.value.bits := MuxCase(
    0.U,
    Seq(
      (io.sourceTag.valid && io.reorderBufferValue.valid) -> io.reorderBufferValue.bits,
      (io.sourceTag.valid && outputMatchingTagExists) -> Mux1H(
        io.outputCollector.outputs.map(o =>
          (o.valid && o.bits.tag === io.sourceTag.bits) -> o.bits.value,
        ),
      ),
      (!io.sourceTag.valid) -> io.registerFileValue,
    ),
  )
}

object ValueSelector {
  def getValue(
    reorderBufferValue: Valid[UInt],
    registerFileValue: UInt,
    outputCollector: CollectedOutput,
    sourceTag: Valid[Tag],
  )(implicit params: Parameters): Valid[UInt] = {
    val m = Module(new ValueSelector)
    m.io.reorderBufferValue := reorderBufferValue
    m.io.registerFileValue := registerFileValue
    m.io.outputCollector := outputCollector
    m.io.sourceTag := sourceTag
    m.io.value
  }
}
