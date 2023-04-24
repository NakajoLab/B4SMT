package b4processor.common

import chisel3._
import chisel3.util.{BitPat, MuxCase, MuxLookup}

/** 命令の種類のチェック */
class InstructionChecker extends Module {
  val input = IO(Input(new Bundle {
    val function3bits = UInt(3.W)
    val function7bits = UInt(7.W)
    val opcode = UInt(7.W)
  }))
  val output = IO(Output(new Bundle {
    val instruction = Instructions()
    val branch = BranchOperations()
    val operationWidth = OperationWidth()
    val arithmetic = ArithmeticOperations()
    val csr = CSROperations()
  }))

  output.instruction := MuxLookup(
    input.opcode,
    Instructions.Unknown,
    Seq(
      // I
      "b0000011".U -> Instructions.Load,
      "b0001111".U -> Mux(
        input.function3bits(0),
        Instructions.fencei,
        Instructions.fence
      ),
      "b1110011".U -> Mux(
        input.function3bits.orR,
        Mux(input.function3bits(2), Instructions.CsrI, Instructions.Csr),
        Mux(input.function7bits(0), Instructions.ebreak, Instructions.ecall)
      ),
      "b0010011".U -> Instructions.ArithmeticImmediate,
      "b0011011".U -> Instructions.ArithmeticImmediate,
      "b1100111".U -> Instructions.jalr,
      // J
      "b1101111".U -> Instructions.jal,
      // U
      "b0110111".U -> Instructions.lui,
      "b0010111".U -> Instructions.auipc,
      // B
      "b1100011".U -> Instructions.Branch,
      // S
      "b0100011".U -> Instructions.Store,
      // R
      "b0110011".U -> Instructions.Arithmetic,
      "b0111011".U -> Instructions.Arithmetic
    )
  )

  output.branch := Mux(
    output.instruction === Instructions.Branch,
    MuxLookup(
      input.function3bits,
      BranchOperations.Unknown,
      Seq(
        0.U -> BranchOperations.Equal,
        1.U -> BranchOperations.NotEqual,
        4.U -> BranchOperations.LessThan,
        5.U -> BranchOperations.GreaterOrEqual,
        6.U -> BranchOperations.LessThanUnsigned,
        7.U -> BranchOperations.GreaterOrEqualUnsigned
      )
    ),
    BranchOperations.Unknown
  )

  output.operationWidth := Mux(
    output.instruction === Instructions.Load || output.instruction === Instructions.Store,
    MuxLookup(
      input.function3bits,
      OperationWidth.Unknown,
      Seq(
        0.U -> OperationWidth.Byte,
        1.U -> OperationWidth.HalfWord,
        2.U -> OperationWidth.Word,
        3.U -> OperationWidth.DoubleWord
      )
    ),
    MuxCase(
      OperationWidth.Unknown,
      Seq(
        (input.opcode === BitPat("b0?10011")) -> OperationWidth.DoubleWord,
        (input.opcode === BitPat("b0?11011")) -> OperationWidth.Word
      )
    )
  )

  output.arithmetic := Mux(
    output.instruction === Instructions.Arithmetic || output.instruction === Instructions.ArithmeticImmediate,
    MuxLookup(
      input.function3bits,
      ArithmeticOperations.Unknown,
      Seq(
        0.U -> Mux(
          output.instruction === Instructions.Arithmetic && input.function7bits(
            5
          ),
          ArithmeticOperations.Subtraction,
          ArithmeticOperations.Addition
        ),
        1.U -> ArithmeticOperations.ShiftLeftLogical,
        2.U -> ArithmeticOperations.SetLessThan,
        3.U -> ArithmeticOperations.SetLessThanUnsigned,
        4.U -> ArithmeticOperations.Xor,
        5.U -> Mux(
          input.function7bits(5),
          ArithmeticOperations.ShiftRightArithmetic,
          ArithmeticOperations.ShiftRightLogical
        ),
        6.U -> ArithmeticOperations.Or,
        7.U -> ArithmeticOperations.And
      )
    ),
    ArithmeticOperations.Unknown
  )

  output.csr := Mux(
    output.instruction === Instructions.Csr || output.instruction === Instructions.CsrI,
    MuxLookup(
      input.function3bits(1, 0),
      CSROperations.Unknown,
      Seq(
        1.U -> CSROperations.ReadAndWrite,
        2.U -> CSROperations.ReadAndSet,
        3.U -> CSROperations.ReadAndClear
      )
    ),
    CSROperations.Unknown
  )
}

/** 命令の種類 */
object Instructions extends ChiselEnum {
  val lui, auipc, jal, jalr, Branch, Load, Store, ArithmeticImmediate,
    Arithmetic, fence, fencei, ecall, ebreak, Csr, CsrI, Unknown = Value
}

/** 分岐命令の種類 */
object BranchOperations extends ChiselEnum {

  /** beq
    *
    * rd = rs1 == rs2
    */
  val Equal = Value

  /** bne
    *
    * rd = rs1 != rs2
    */
  val NotEqual = Value

  /** blt
    *
    * rd = rs1 &lt; rs2
    */
  val LessThan = Value

  /** bge
    *
    * rd = rs1 &gt;= rs2
    */
  val GreaterOrEqual = Value

  /** bltu
    *
    * rd = rs1 &lt; rs2 unsigned
    */
  val LessThanUnsigned = Value

  /** bgeu
    *
    * rd = rs1 &gt;= rs2 unsigned
    */
  val GreaterOrEqualUnsigned = Value

  /** Unknown */
  val Unknown = Value
}

/** 操作するビット幅 (演算、load、store) */
object OperationWidth extends ChiselEnum {

  /** 8bit */
  val Byte = Value

  /** 16bit */
  val HalfWord = Value

  /** 32bit */
  val Word = Value

  /** 64bit */
  val DoubleWord = Value

  /** Unknown */
  val Unknown = Value
}

/** 演算の種類 */
object ArithmeticOperations extends ChiselEnum {

  /** add, addi */
  val Addition = Value

  /** sub */
  val Subtraction = Value

  /** sll, slli */
  val ShiftLeftLogical = Value

  /** slt, slti */
  val SetLessThan = Value

  /** sltu, sltiu */
  val SetLessThanUnsigned = Value

  /** xor, xori */
  val Xor = Value

  /** srl, srli */
  val ShiftRightLogical = Value

  /** sra, srai */
  val ShiftRightArithmetic = Value

  /** or, ori */
  val Or = Value

  /** and, andi */
  val And = Value

  /** Unknown */
  val Unknown = Value
}

/** CSRへの操作の種類 */
object CSROperations extends ChiselEnum {

  /** csrrw CSRの中身をrdに書き出し、rsの内容をCSRに書き込む */
  val ReadAndWrite = Value

  /** csrrs CSRの中身をrdに書き出し、rsをビットマスクとしてビットを増やすようにCSR|=rsのように書き込む */
  val ReadAndSet = Value

  /** csrrc CSRの中身をrdに書き出し、rsをビットマスクとしてビットを消すようにCSR&=rsのように書き込む */
  val ReadAndClear = Value

  /** Unknown */
  val Unknown = Value
}
