package b4smt.utils

import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec

class InstructionUtilTest extends AnyFlatSpec {
  behavior of "InstructionUtil"

  it should "pack bytes little-endian and pad only the final word" in {
    assert(
      InstructionUtil
        .fromStringSeq8bit(Seq("x01", "02", "03"))
        .map(_.litValue) ==
        Seq(BigInt("030201", 16)),
    )
    val bytes = (1 to 11).map(n => f"$n%02X")
    assert(
      InstructionUtil.fromStringSeq8bit(bytes).map(_.litValue) ==
        Seq(BigInt("0807060504030201", 16), BigInt("0b0a09", 16)),
    )
    assert(InstructionUtil.fromStringSeq8bit(Seq.empty).isEmpty)
  }

  it should "preserve 32-bit and 64-bit word ordering" in {
    assert(
      InstructionUtil
        .fromStringSeq32bit(Seq("x04030201", "08070605", "0c0b0a09"))
        .map(_.litValue) ==
        Seq(BigInt("0807060504030201", 16), BigInt("0c0b0a09", 16)),
    )
    assert(
      InstructionUtil
        .fromStringSeq64bit(Seq("x0807060504030201", "1817161514131211"))
        .map(_.litValue) ==
        Seq(BigInt("0807060504030201", 16), BigInt("1817161514131211", 16)),
    )
  }

  it should "load hex files and unsigned binary bytes" in {
    val file = Files.createTempFile("b4smt-instructions", ".tmp")
    try {
      Files.writeString(file, "x01\n\n02\n03\n")
      assert(
        InstructionUtil.fromFile8bit(file.toString).head.litValue == 0x030201,
      )
      Files.writeString(file, "x04030201\n08070605\n")
      assert(
        InstructionUtil.fromFile32bit(file.toString).head.litValue ==
          BigInt("0807060504030201", 16),
      )
      Files.writeString(file, "x0807060504030201\n")
      assert(
        InstructionUtil.fromFile64bit(file.toString).head.litValue ==
          BigInt("0807060504030201", 16),
      )
      Files.write(file, Array[Byte](0x80.toByte, 1, 0xff.toByte))
      assert(
        InstructionUtil.fromBinaryFile(file.toString).head.litValue == 0xff0180,
      )
    } finally Files.delete(file)
  }
}
