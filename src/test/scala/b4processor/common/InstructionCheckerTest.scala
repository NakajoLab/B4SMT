package b4processor.common

import b4processor.common.ArithmeticOperations._
import b4processor.common.BranchOperations._
import b4processor.common.CSROperations._
import b4processor.common.Instructions._
import b4processor.common.OperationWidth._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class InstructionCheckerWrapper extends InstructionChecker {
  def setInstruction(instruction: UInt): Unit = {
    val opcode = instruction(6, 0)
    val funct3 = instruction(14, 12)
    val funct7 = instruction(31, 25)

    this.input.opcode.poke(opcode)
    this.input.function3bits.poke(funct3)
    this.input.function7bits.poke(funct7)
  }

  def expect(
    instruction: Instructions.Type = Instructions.Unknown,
    branch: BranchOperations.Type = BranchOperations.Unknown,
    operationWidth: OperationWidth.Type = OperationWidth.Unknown,
    arithmetic: ArithmeticOperations.Type = ArithmeticOperations.Unknown,
    csr: CSROperations.Type = CSROperations.Unknown,
  ): Unit = {
    this.output.instruction.expect(instruction, s"$instruction")
    this.output.branch.expect(branch, s"$branch")
    this.output.operationWidth.expect(operationWidth, s"$operationWidth")
    this.output.arithmetic.expect(arithmetic, s"$arithmetic")
    this.output.csr.expect(csr, s"$csr")
  }
}

class InstructionCheckerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "InstructionChecker"

  it should "parse add x1, x2, x3" in {
    test(new InstructionCheckerWrapper) { c =>
      c.setInstruction("x003100b3".U)
      c.expect(
        instruction = Arithmetic,
        arithmetic = Addition,
        operationWidth = DoubleWord,
      )
    }
  }

  it should "parse addw x1, x2, x3" in {
    test(new InstructionCheckerWrapper) { c =>
      c.setInstruction("x003100bb".U)
      c.expect(
        instruction = Arithmetic,
        arithmetic = Addition,
        operationWidth = Word,
      )
    }
  }

  it should "parse sb x1, 5(x2)" in {
    test(new InstructionCheckerWrapper) { c =>
      c.setInstruction("x001102a3".U)
      c.expect(instruction = Store, operationWidth = Byte)
    }
  }

  it should "parse csrrw x1, mhartid, x2" in {
    test(new InstructionCheckerWrapper) { c =>
      c.setInstruction("xf14110f3".U)
      c.expect(instruction = Csr, csr = ReadAndWrite)
    }
  }

  it should "parse bge x1, x2, 0" in {
    test(new InstructionCheckerWrapper) { c =>
      c.setInstruction("x0020d263".U)
      c.expect(instruction = Branch, branch = GreaterOrEqual)
    }
  }
}
