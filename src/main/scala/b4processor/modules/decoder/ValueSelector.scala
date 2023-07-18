package b4processor.modules.decoder

import b4processor.Parameters
import b4processor.connections.CollectedOutput
import b4processor.utils.Tag
import chisel3._
import chisel3.util._

/** ソースタグ1の値を選択する回路
  *
  * @param params
  *   パラメータ
  */
class ValueSelector(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val reorderBufferValue = Flipped(Valid(UInt(64.W)))
    val registerFileValue = Input(UInt(64.W))
    val outputCollector = Flipped(new CollectedOutput)
    val sourceTag = Input(Valid(new Tag))
    val value = Valid(UInt(64.W))
  })

  // ALUからバイパスされた値のうち、destination tagと一致するsource tagを持っている
  private val outputMatchingTagExists = io.outputCollector.outputs
    .map(o => o.valid && o.bits.tag === io.sourceTag.bits)
    .fold(false.B)(_ || _)

  // 値があるか
  io.value.valid := MuxCase(
    false.B,
    Seq(
      (io.sourceTag.valid && io.reorderBufferValue.valid) -> true.B,
      (io.sourceTag.valid && outputMatchingTagExists) -> true.B,
      (!io.sourceTag.valid) -> true.B
    )
  )
  // 値の内容
  io.value.bits := MuxCase(
    0.U,
    Seq(
      (io.sourceTag.valid && io.reorderBufferValue.valid) -> io.reorderBufferValue.bits,
      (io.sourceTag.valid && outputMatchingTagExists) -> Mux1H(
        io.outputCollector.outputs.map(o =>
          (o.valid && o.bits.tag === io.sourceTag.bits) -> o.bits.value
        )
      ),
      (!io.sourceTag.valid) -> io.registerFileValue
    )
  )
}

object ValueSelector {
  def getValue(
    reorderBufferValue: Valid[UInt],
    registerFileValue: UInt,
    outputCollector: CollectedOutput,
    sourceTag: Valid[Tag]
  )(implicit params: Parameters): Valid[UInt] = {
    val m = Module(new ValueSelector)
    m.io.reorderBufferValue := reorderBufferValue
    m.io.registerFileValue := registerFileValue
    m.io.outputCollector := outputCollector
    m.io.sourceTag := sourceTag
    m.io.value
  }
}
