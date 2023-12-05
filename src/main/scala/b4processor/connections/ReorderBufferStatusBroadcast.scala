package b4processor.connections

import b4processor.Parameters
import b4processor.utils.Tag
import chisel3._
import chisel3.util._

/** リオーダバッファからの命令のステータス通知． インオーダ発行が必要な命令や，命令のキャンセル通知に使用
  */
object InstructionStatus extends ChiselEnum {

  /** 実行中（通知に使用されずに各モジュールの状態管理に使用） */
  val Running = Value

  /** 実行が確定した命令（store等で使用） */
  val Confirmed = Value

  /** 実行完了 */
  val Done = Value

  /** 命令実行がキャンセルされた命令 */
  val Canceled = Value
}

/** LSQとリオーダバッファをつなぐ
  */
class ReorderBufferStatusBroadcast(implicit params: Parameters) extends Bundle {
  val instructions = Vec(
    params.maxRegisterFileCommitCount,
    Valid(new Bundle {
      val destinationTag = new Tag
      val status = InstructionStatus()
    }),
  )
  val isError = Bool()
}
