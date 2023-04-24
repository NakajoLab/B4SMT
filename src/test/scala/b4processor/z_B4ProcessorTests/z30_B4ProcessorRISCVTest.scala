package b4processor.z_B4ProcessorTests

import b4processor.Parameters
import b4processor.utils.B4ProcessorWithMemory
import chiseltest._
import chiseltest.internal.CachingAnnotation
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.tagobjects.Slow

class B4ProcessorRISCVTestWrapper()(implicit params: Parameters)
    extends B4ProcessorWithMemory() {
  def riscv_test(): Unit = {
    while (this.io.registerFileContents.get(0)(17).peekInt() != 93) {
      this.clock.step()
    }
    val reg3_value = this.io.registerFileContents.get(0)(3).peekInt()
    val fail_num = reg3_value >> 1
    this.io.registerFileContents
      .get(0)(3)
      .expect(1, s"failed on test ${fail_num}")
  }
}

class B4ProcessorRISCVTest extends AnyFlatSpec with ChiselScalatestTester {
  // デバッグに時間がかかりすぎるのでパラメータを少し下げる。
  implicit val defaultParams = {
    Parameters(
      debug = true,
      threads = 1,
      decoderPerThread = 1,
      tagWidth = 4,
      loadStoreQueueIndexWidth = 2,
      maxRegisterFileCommitCount = 2,
      instructionStart = 0x8000_0000L
    )
  }
  val backendAnnotation = IcarusBackendAnnotation
  val WriteWaveformAnnotation = WriteFstAnnotation

  behavior of s"RISC-V tests rv64i"

  def riscv_test_i(test_name: String, timeout: Int = 2000): Unit = {

    it should s"run risc-v test ${test_name}" taggedAs (RISCVTest, Slow) in {
      test( // FIXME fromFile8bit
        new B4ProcessorRISCVTestWrapper(
        )
      )
        .withAnnotations(
          Seq(WriteWaveformAnnotation, CachingAnnotation, backendAnnotation)
        ) { c =>
          c.clock.setTimeout(timeout)
          c.initialize(
            s"programs/riscv-tests/share/riscv-tests/isa/rv64ui-p-${test_name}"
          )
          c.riscv_test()
        }
    }
  }

  riscv_test_i("add")
  riscv_test_i("addi")
  riscv_test_i("addiw")
  riscv_test_i("addw")
  riscv_test_i("and")
  riscv_test_i("andi")
  riscv_test_i("auipc")
  riscv_test_i("beq")
  riscv_test_i("bge")
  riscv_test_i("bgeu")
  riscv_test_i("blt")
  riscv_test_i("bltu")
  riscv_test_i("bne")
  riscv_test_i("fence_i")
  riscv_test_i("jal")
  riscv_test_i("jalr")
  riscv_test_i("lb")
  riscv_test_i("lbu")
  riscv_test_i("ld")
  riscv_test_i("lh")
  riscv_test_i("lhu")
  riscv_test_i("lui")
  riscv_test_i("lw")
  riscv_test_i("lwu")
  riscv_test_i("or")
  riscv_test_i("ori")
  riscv_test_i("sb")
  riscv_test_i("sd")
  riscv_test_i("sh")
  riscv_test_i("sll")
  riscv_test_i("slli")
  riscv_test_i("slliw")
  riscv_test_i("sllw")
  riscv_test_i("slt")
  riscv_test_i("slti")
  riscv_test_i("sltiu")
  riscv_test_i("sltu")
  riscv_test_i("sra")
  riscv_test_i("srai")
  riscv_test_i("sraiw")
  riscv_test_i("sraw")
  riscv_test_i("srl")
  riscv_test_i("srli")
  riscv_test_i("srliw")
  riscv_test_i("srlw")
  riscv_test_i("sub")
  riscv_test_i("subw")
  riscv_test_i("sw")
  riscv_test_i("xor")
  riscv_test_i("xori")

  behavior of s"RISC-V tests rv64c"

  object RISCVTest extends Tag("RISCVTests")

  def riscv_test_c(test_name: String, timeout: Int = 2000): Unit = {

    it should s"run risc-v test ${test_name}" taggedAs (RISCVTest, Slow) in {
      test( // FIXME fromFile8bit
        new B4ProcessorRISCVTestWrapper(
        )
      )
        .withAnnotations(
          Seq(WriteWaveformAnnotation, CachingAnnotation, backendAnnotation)
        ) { c =>
          c.clock.setTimeout(timeout)
          c.initialize(
            s"programs/riscv-tests/share/riscv-tests/isa/rv64uc-p-${test_name}"
          )
          c.riscv_test()
        }
    }
  }

  riscv_test_c("rvc")
}
