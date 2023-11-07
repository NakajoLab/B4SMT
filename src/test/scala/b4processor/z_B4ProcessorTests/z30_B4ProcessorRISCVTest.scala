package b4processor.z_B4ProcessorTests

import b4processor.Parameters
import b4processor.utils.B4ProcessorWithMemory
import chiseltest._
import chiseltest.internal.CachingAnnotation
import firrtl2.annotations.Annotation
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
    val error_message =
      s"failed on test $fail_num\n" + this.io.registerFileContents
        .get(0)
        .map(_.peekInt())
        .zipWithIndex
        .map { case (n, i) => f"x$i%-2d = ($n%016X) $n" }
        .reduce(_ + "\n" + _)
    this.io.registerFileContents
      .get(0)(3)
      .expect(1, error_message)
  }
}

abstract class RiscvTest(val testPrefix: String)
    extends AnyFlatSpec
    with ChiselScalatestTester {
  var params = Parameters(
    debug = true,
    threads = 1,
    decoderPerThread = 4,
    tagWidth = 4,
    loadStoreQueueIndexWidth = 2,
    maxRegisterFileCommitCount = 2,
    instructionStart = 0x8000_0000L,
//    enablePExt = true
  )

  val annotation = IcarusBackendAnnotation
  val writeWaveform = WriteFstAnnotation

  object RISCVTest extends Tag("RISCVTest")

  def riscv_test(
    test_name: String,
    timeout: Int = 2000,
    backendAnnotation: Annotation = annotation,
  ): Unit = {

    it should s"run $test_name" taggedAs (RISCVTest, Slow) in {
      test( // FIXME fromFile8bit
        new B4ProcessorRISCVTestWrapper(
        )(this.params),
      )
        .withAnnotations(
          Seq(backendAnnotation, CachingAnnotation, this.writeWaveform),
        ) { c =>
          c.clock.setTimeout(timeout)
          c.initialize(
            s"programs/riscv-tests/share/riscv-tests/isa/rv64u$testPrefix-p-$test_name",
          )
          c.riscv_test()
        }
    }
  }
}

class RiscvTestI extends RiscvTest("i") {
  behavior of s"riscv_test_i"

  riscv_test("add")
  riscv_test("addi")
  riscv_test("addiw")
  riscv_test("addw")
  riscv_test("and")
  riscv_test("andi")
  riscv_test("auipc")
  riscv_test("beq")
  riscv_test("bge")
  riscv_test("bgeu")
  riscv_test("blt")
  riscv_test("bltu")
  riscv_test("bne")
  riscv_test("fence_i")
  riscv_test("jal")
  riscv_test("jalr")
  riscv_test("lb")
  riscv_test("lbu")
  riscv_test("ld")
  riscv_test("lh")
  riscv_test("lhu")
  riscv_test("lui")
  riscv_test("lw")
  riscv_test("lwu")
  riscv_test("or")
  riscv_test("ori")
  riscv_test("sb")
  riscv_test("sd")
  riscv_test("sh")
  riscv_test("sll")
  riscv_test("slli")
  riscv_test("slliw")
  riscv_test("sllw")
  riscv_test("slt")
  riscv_test("slti")
  riscv_test("sltiu")
  riscv_test("sltu")
  riscv_test("sra")
  riscv_test("srai")
  riscv_test("sraiw")
  riscv_test("sraw")
  riscv_test("srl")
  riscv_test("srli")
  riscv_test("srliw")
  riscv_test("srlw")
  riscv_test("sub")
  riscv_test("subw")
  riscv_test("sw")
  riscv_test("xor")
  riscv_test("xori")
}

class RiscvTestC extends RiscvTest("c") {
  behavior of "riscv_test_c"
  riscv_test("rvc")
}

class RiscvTestA extends RiscvTest("a") {
  behavior of "riscv_test_a"

  riscv_test("lrsc", timeout = 100000, VerilatorBackendAnnotation)

  riscv_test("amoadd_w")
  riscv_test("amoand_w")
  riscv_test("amomax_w")
  riscv_test("amomaxu_w")
  riscv_test("amomin_w")
  riscv_test("amominu_w")
  riscv_test("amoor_w")
  riscv_test("amoswap_w")
  riscv_test("amoxor_w")

  riscv_test("amoadd_d")
  riscv_test("amoand_d")
  riscv_test("amomax_d")
  riscv_test("amomaxu_d")
  riscv_test("amomin_d")
  riscv_test("amominu_d")
  riscv_test("amoor_d")
  riscv_test("amoswap_d")
  riscv_test("amoxor_d")
}
