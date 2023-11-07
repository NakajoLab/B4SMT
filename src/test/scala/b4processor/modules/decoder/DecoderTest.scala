package b4processor.modules.decoder

import b4processor.Parameters
import b4processor.utils.RVRegister.{AddRegConstructor, AddUIntRegConstructor}
import b4processor.utils.{ExecutorValue, RVRegister, SymbiYosysFormal, Tag}
import chisel3._
import chiseltest._
import chiseltest.experimental.sanitizeFileName
import chiseltest.internal.TestEnvInterface
import circt.stage.ChiselStage
import org.scalatest.{
  Assertions,
  Outcome,
  Suite,
  TestData,
  TestSuite,
  TestSuiteMixin,
}
import org.scalatest.flatspec.AnyFlatSpec

import java.io.PrintWriter
import scala.reflect.io.{Directory, File}
import scala.util.DynamicVariable
import sys.process._

/** デコーダをテストしやすくするためにラップしたもの
  */
class DecoderWrapper(implicit params: Parameters) extends Decoder {

  def initialize(instruction: UInt, programCounter: Int = 1000): Unit = {
    this.setImem(instruction, programCounter)
    this.setReorderBuffer()
    this.setRegisterFile()
    this.setLoadStoreQueueReady()
    this.setOutputs()
    this.io.reservationStation.ready.poke(true)
    this.io.csr.ready.poke(true)
    this.io.amo.ready.poke(true)
  }

  def setImem(
    instruction: UInt,
    programCounter: Int,
    isPrediction: Boolean = false,
  ): Unit = {
    this.io.instructionFetch.bits.instruction.poke(instruction)
    this.io.instructionFetch.bits.programCounter.poke(programCounter)
    this.io.instructionFetch.valid.poke(true)
  }

  def setReorderBuffer(
    destinationTag: Int = 0,
    sourceTag1: Option[Int] = None,
    value1: Option[Int] = None,
    sourceTag2: Option[Int] = None,
    value2: Option[Int] = None,
  ): Unit = {
    this.io.reorderBuffer.destination.destinationTag
      .poke(Tag(0, destinationTag))
    this.io.reorderBuffer
      .sources(0)
      .matchingTag
      .valid
      .poke(sourceTag1.isDefined)
    this.io.reorderBuffer
      .sources(0)
      .matchingTag
      .bits
      .poke(Tag(0, sourceTag1.getOrElse(0)))
    this.io.reorderBuffer.sources(0).value.valid.poke(value1.isDefined)
    this.io.reorderBuffer.sources(0).value.bits.poke(value1.getOrElse(0))
    this.io.reorderBuffer
      .sources(1)
      .matchingTag
      .valid
      .poke(sourceTag2.isDefined)
    this.io.reorderBuffer
      .sources(1)
      .matchingTag
      .bits
      .poke(Tag(0, sourceTag2.getOrElse(0)))
    this.io.reorderBuffer.sources(1).value.valid.poke(value2.isDefined)
    this.io.reorderBuffer.sources(1).value.bits.poke(value2.getOrElse(0))
    this.io.reorderBuffer.ready.poke(true)
  }

  def setRegisterFile(value1: Int = 0, value2: Int = 0): Unit = {
    this.io.registerFile.values(0).poke(value1)
    this.io.registerFile.values(1).poke(value2)
  }

  def setOutputs(bypassedValues: Option[ExecutorValue] = None): Unit = {
    this.io.outputCollector
      .outputs(0)
      .valid
      .poke(bypassedValues.isDefined)
    this.io.outputCollector
      .outputs(0)
      .bits
      .tag
      .poke(
        Tag(
          0,
          bypassedValues
            .getOrElse(ExecutorValue(destinationTag = 0, value = 0))
            .destinationTag,
        ),
      )
    this.io.outputCollector
      .outputs(0)
      .bits
      .value
      .poke(
        bypassedValues
          .getOrElse(ExecutorValue(destinationTag = 0, value = 0))
          .value,
      )
  }

  def setLoadStoreQueueReady(ready: Boolean = true): Unit = {
    this.io.loadStoreQueue.ready.poke(ready)
  }

  def expectReorderBuffer(
    destinationRegister: RVRegister = RVRegister(0),
    sourceRegister1: RVRegister = RVRegister(0),
    sourceRegister2: RVRegister = RVRegister(0),
  ): Unit = {
    // check rd
    this.io.reorderBuffer.destination.destinationRegister
      .expect(destinationRegister, "rdの値が間違っています")
    // check rs1
    this.io.reorderBuffer
      .sources(0)
      .sourceRegister
      .expect(sourceRegister1, "rs1 doesn't match")
    // check rs2
    this.io.reorderBuffer
      .sources(1)
      .sourceRegister
      .expect(sourceRegister2, "rs2 doesn't match")
  }

  def expectReservationStation(
    destinationTag: Int = 0,
    sourceTag1: Int = 0,
    sourceTag2: Int = 0,
    value1: Int = 0,
    value2: Int = 0,
    immediateOrFunction7: Int = 0,
  ): Unit = {
    this.io.reservationStation.entry.destinationTag
      .expect(Tag(0, destinationTag))
    // TODO
//    this.io.reservationStation.entry.sources(0).getTag.expect(Tag(0, sourceTag1))
//    this.io.reservationStation.entry.sources(1).getTag.expect(Tag(0, sourceTag2))
//    this.io.reservationStation.entry.sources(0)..expect(value1)
//    this.io.reservationStation.entry.value2.expect(value2)
  }

  def expectCSR(destinationTag: Int, value: Int, valueReady: Boolean): Unit = {
    import this.io.csr._
    valid.expect(true)
    bits.destinationTag.expect(Tag(0, destinationTag))
    bits.value.expect(value)
    bits.ready.expect(valueReady)
  }
}

/** デコーダのテスト
  */
class DecoderTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with SymbiYosysFormal {
  behavior of "decoder"
  implicit val testParams =
    Parameters(threads = 1, decoderPerThread = 1)

  // rs1 rs2 rdが正しくリオーダバッファに渡されているか
  it should "pass rs1 rs2 rd to reorder buffer" in {
    test(new DecoderWrapper) { c =>
      // add x1,x2,x3
      c.initialize("x003100b3".U)
      c.expectReorderBuffer(
        destinationRegister = 1.reg,
        sourceRegister1 = 2.reg,
        sourceRegister2 = 3.reg,
      )
    }
  }

  // レジスタファイルから値を取得できているか
  it should "get values from register file" in {
    test(new DecoderWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // add x1,x2,x3
      c.initialize("x003100b3".U)
      c.setRegisterFile(value1 = 10, value2 = 20)

      c.clock.step()

      c.expectReorderBuffer(
        destinationRegister = 1.reg,
        sourceRegister1 = 2.reg,
        sourceRegister2 = 3.reg,
      )
      c.expectReservationStation(value1 = 10, value2 = 20)
    }
  }

  // stagをリオーダバッファから取得できているか
  it should "get source tags from reorder buffer" in {
    test(new DecoderWrapper) { c =>
      // add x1,x2,x3
      c.initialize("x003100b3".U)
      c.setReorderBuffer(
        destinationTag = 5,
        sourceTag1 = Some(6),
        sourceTag2 = Some(7),
      )

      c.expectReorderBuffer(
        destinationRegister = 1.reg,
        sourceRegister1 = 2.reg,
        sourceRegister2 = 3.reg,
      )
      c.expectReservationStation(
        destinationTag = 5,
        sourceTag1 = 6,
        sourceTag2 = 7,
      )
    }
  }

  // stagと値をリオーダバッファから取得できているか
  it should "get source tags and values from reorder buffer" in {
    test(new DecoderWrapper) { c =>
      // add x1,x2,x3
      c.initialize("x003100b3".U)
      c.setReorderBuffer(
        destinationTag = 5,
        sourceTag1 = Some(6),
        sourceTag2 = Some(7),
        value1 = Some(20),
        value2 = Some(21),
      )

      c.expectReorderBuffer(
        destinationRegister = 1.reg,
        sourceRegister1 = 2.reg,
        sourceRegister2 = 3.reg,
      )
      c.expectReservationStation(destinationTag = 5, value1 = 20, value2 = 21)
    }
  }

  // sd命令を認識できる
  it should "understand sd" in {
    test(new DecoderWrapper) { c =>
      // sd x1,10(x2)
      c.initialize("x00113523".U)
      c.expectReorderBuffer(sourceRegister1 = 2.reg, sourceRegister2 = 1.reg)
      c.expectReservationStation()
    }
  }

  // I形式を認識できる
  it should "understand I" in {
    test(new DecoderWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // addi x1,x2,20
      c.initialize("x01410093".U)
      c.setReorderBuffer(
        destinationTag = 5,
        sourceTag1 = Some(6),
        sourceTag2 = Some(7),
      )

      c.expectReorderBuffer(1.reg, sourceRegister1 = 2.reg)
      // TODO show
//      println(
//        c.io.reservationStation.entry.value2.peekInt(),
//        c.io.reservationStation.entry.ready2.peekInt(),
//        c.io.reservationStation.entry.sourceTag2.peek()
//      )
      c.expectReservationStation(
        destinationTag = 5,
        sourceTag1 = 6,
        value2 = 20,
      )
      c.clock.step(1)
    }
  }

  // ALUからの値を使える
  it should "do register bypass" in {
    test(
      new DecoderWrapper()(testParams.copy(threads = 1, decoderPerThread = 2)),
    ) { c =>
      // add x1,x2,x3
      c.initialize("x003100b3".U)
      c.setReorderBuffer(
        destinationTag = 5,
        sourceTag1 = Some(6),
        sourceTag2 = Some(7),
      )
      c.setOutputs(Some(ExecutorValue(6, 20)))

      c.expectReorderBuffer(
        destinationRegister = 1.reg,
        sourceRegister1 = 2.reg,
        sourceRegister2 = 3.reg,
      )
      c.expectReservationStation(
        destinationTag = 5,
        value1 = 20,
        sourceTag2 = 7,
      )
    }
  }

  // imemがvalidのときRSとRBでvalidと表示されている
  it should "say the data is valid when imem is valid" in {
    test(new DecoderWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // add x1,x2,x3
      c.initialize("x003100b3".U)

      c.clock.step(3)

      c.io.reorderBuffer.valid.expect(true.B)
      c.io.reservationStation.entry.valid.expect(true.B)
    }
  }

  // imemがvalid=0のときRSとRBでvalid=0と表示されている
  it should "say the data is invalid when imem invalid" in {
    test(new DecoderWrapper) { c =>
      c.initialize(0.U)
      c.io.instructionFetch.valid.poke(false.B)

      c.io.reorderBuffer.valid.expect(false.B)
      c.io.reservationStation.entry.valid.expect(false.B)
    }
  }

  // U形式を認識 (edge case: "x0007b1b7".U)
  it should "understand U format" in {
    test(new DecoderWrapper()(testParams)) { c =>
      // lui x3, 123
      c.initialize("x0007b1b7".U)
      c.setReorderBuffer(destinationTag = 5)

      c.expectReorderBuffer(destinationRegister = 3.reg)
      c.expectReservationStation(
        destinationTag = 5,
        value1 = 503808,
        value2 = 0,
      )
    }
  }

  it should "understand J format" in {
    test(new DecoderWrapper()(testParams)) { c =>
      // jal x10,LABEL
      // LABEL:
      c.initialize("x0040056f".U)
      c.setReorderBuffer(destinationTag = 5)

      c.expectReorderBuffer(destinationRegister = 10.reg)
      c.expectReservationStation(destinationTag = 5, value1 = 4, value2 = 1000)
    }
  }

  it should "do csr" in {
    test(new DecoderWrapper()(testParams)) { c =>
      // jal x10,LABEL
      // LABEL:
      c.initialize("xc0002573".U)
      c.setReorderBuffer(destinationTag = 5)

      c.expectReorderBuffer(destinationRegister = 10.reg)
      c.expectCSR(destinationTag = 5, value = 0, valueReady = true)
    }
  }

  it should "pass isPrediction" in {
    test(new DecoderWrapper()(testParams)) { c =>
      c.initialize("x0040056f".U)
      c.setImem("x0040056f".U, 0, isPrediction = true)
    }
  }

  it should "do pext" in {
    test(new DecoderWrapper()(testParams.copy(enablePExt = true))) { c =>
      c.initialize("x40aaacf7".U) // ADD32 x25,x21,x10
      c.setImem("x40aaacf7".U, 0)

      c.expectReorderBuffer(
        destinationRegister = 25.reg,
        sourceRegister1 = 21.reg,
        sourceRegister2 = 10.reg,
      )
    }
  }

  it should "check formal" in {
    symbiYosysCheck(new DecoderWrapper())
  }

  it should "check formal with pext" in {
    symbiYosysCheck(new DecoderWrapper()(testParams.copy(enablePExt = true)))
  }
}
