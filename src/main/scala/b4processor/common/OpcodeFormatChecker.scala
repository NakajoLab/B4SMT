package b4processor.common

import chisel3._
import chisel3.util._

object OpcodeFormat extends ChiselEnum {
  val R, I, J, S, U, B, Unknown = Value
}

class OpcodeFormatChecker extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(7.W))
    val format = Output(OpcodeFormat())
  })

  // RV32I対応
  io.format := MuxLookup(
    io.opcode,
    OpcodeFormat.Unknown,
    Seq(
      // I
      "b0000011".U -> OpcodeFormat.I, // load
      "b0001111".U -> OpcodeFormat.I, // fence
      "b1110011".U -> OpcodeFormat.I, // csr, ecall, ebreak
      "b0010011".U -> OpcodeFormat.I, // 演算
      "b0011011".U -> OpcodeFormat.I, // 演算(64I)
      "b1100111".U -> OpcodeFormat.I, // jalr
      // J
      "b1101111".U -> OpcodeFormat.J, // jal
      // U
      "b0110111".U -> OpcodeFormat.U, // lui
      "b0010111".U -> OpcodeFormat.U, // aupic
      // B
      "b1100011".U -> OpcodeFormat.B, // 分岐
      // S
      "b0100011".U -> OpcodeFormat.S, // store
      // R
      "b0110011".U -> OpcodeFormat.R, // 演算
      "b0111011".U -> OpcodeFormat.R // 演算(64I)
    )
  )
}
