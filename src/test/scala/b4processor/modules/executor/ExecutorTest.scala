package b4processor.modules.executor

import b4processor.Parameters
import b4processor.connections.{BranchOutput, ResultType}
import b4processor.utils.{
  ExecutorValue,
  FetchValue,
  LSQValue,
  ReservationValue,
  Tag
}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util.Irrevocable
import org.scalatest.GivenWhenThen

class ExecutorWrapper(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val reservationStation = Flipped(new ReservationStation2ExecutorForTest)
    val out = new ExecutionRegisterBypassForTest
    val fetch = Irrevocable(new BranchOutput)
  })

  val executor = Module(new Executor)
  executor.io.reservationStation.bits.destinationTag := io.reservationStation.bits.destinationTag
  executor.io.reservationStation.bits.value1 := io.reservationStation.bits.value1.asUInt
  executor.io.reservationStation.bits.value2 := io.reservationStation.bits.value2.asUInt
  executor.io.reservationStation.bits.function3 := io.reservationStation.bits.function3
  executor.io.reservationStation.bits.immediateOrFunction7 := io.reservationStation.bits.immediateOrFunction7
  executor.io.reservationStation.bits.opcode := io.reservationStation.bits.opcode
  executor.io.reservationStation.bits.wasCompressed := io.reservationStation.bits.wasCompressed
  executor.io.reservationStation.valid := io.reservationStation.valid
  executor.io.out.ready := io.out.ready
  io.reservationStation.ready := executor.io.reservationStation.ready

  io.out.value := executor.io.out.bits.value.asSInt
  io.out.valid := executor.io.out.valid
  io.out.resultType := executor.io.out.bits.resultType
  io.out.destinationTag := executor.io.out.bits.tag

  executor.io.fetch <> io.fetch

  def setALU(values: ReservationValue): Unit = {
    val reservationstation = this.io.reservationStation
    reservationstation.valid.poke(values.valid)
    reservationstation.bits.destinationTag.poke(Tag(0, values.destinationTag))
    reservationstation.bits.wasCompressed.poke(values.wasCompressed)

    /** マイナスの表現ができていない */

    reservationstation.bits.value1.poke(values.value1)
    reservationstation.bits.value2.poke(values.value2)
    reservationstation.bits.function3.poke(values.function3)
    reservationstation.bits.immediateOrFunction7.poke(
      values.immediateOrFunction7
    )
    reservationstation.bits.opcode.poke(values.opcode)
  }

  def expectout(values: Option[ExecutorValue], valid: Boolean = true): Unit = {
    val out = this.io.out
    out.valid.expect(valid)
    if (values.isDefined) {
      out.resultType.expect(ResultType.Result)
      out.destinationTag.expect(Tag(0, values.get.destinationTag))
      out.value.expect(values.get.value)
    }
  }

  def expectLSQ(values: Option[ExecutorValue], valid: Boolean = true): Unit = {
    val out = this.io.out
    out.valid.expect(valid)
    if (values.isDefined) {
      out.resultType.expect(ResultType.LoadStoreAddress)
      out.destinationTag.expect(Tag(0, values.get.destinationTag))
      out.value.expect(values.get.value)
    }
  }

  def expectFetch(values: FetchValue): Unit = {
    val fetch = this.io.fetch
    fetch.valid.expect(values.valid)
    fetch.bits.programCounterOffset.expect(values.programCounter)
  }
}

class ExecutorTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with GivenWhenThen {
  behavior of "Executor"

  implicit val defaultParams = Parameters(decoderPerThread = 1)

  it should "be compatible with I extension" in {
    test(new ExecutorWrapper) { c =>
      c.io.fetch.ready.poke(true)
      c.io.out.ready.poke(true)
      When("lui")
      // rs1 = 40, rs2 = fffff
      c.setALU(
        ReservationValue(
          destinationTag = 10,
          value1 = 40 << 12,
          value2 = 100,
          opcode = 55
        )
      )
      c.expectout(values =
        Some(ExecutorValue(destinationTag = 10, value = 40 << 12))
      )
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("auipc")
      // rs1 = 40, rs2 = 16
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40 << 12,
          value2 = 1000,
          opcode = 55
        )
      )
      c.expectout(values =
        Some(ExecutorValue(destinationTag = 10, value = 40 << 12))
      )
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("jal")
      // rs1 = 40, rs2 = 16
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40, // imm
          value2 = 100, // PC
          opcode = 111
        )
      )
      c.expectout(values =
        Some(ExecutorValue(destinationTag = 10, value = 104))
      )
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("jalr")
      // rs1 = 40, rs2(extend_offset) = 16
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 104,
          value2 = 100,
          immediateOrFunction7 = 16,
          opcode = 103
        )
      )
      c.expectout(values =
        Some(ExecutorValue(destinationTag = 10, value = 104))
      )
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 20))

      When("beq -- NG")
      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))

      When("beq -- OK")
      // rs1 = 40, rs = 40, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 40,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))

      When("bne -- NG")
      // rs1 = 40, rs = 40, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 40,
          function3 = 1,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))

      When("bne -- OK")
      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 1,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))

      When("blt -- NG")
      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 4,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))

      When("blt -- OK")
      // rs1 = 20, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 20,
          value2 = 30,
          function3 = 4,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))

      When("bge -- NG")
      // rs1 = 20, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 20,
          value2 = 30,
          function3 = 5,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))

      When("bge -- OK")
      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 5,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))

      When("bltu -- NG")
      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 6,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))

      When("bltu -- OK")
      // rs1 = 20, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 20,
          value2 = 30,
          function3 = 6,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))

      When("bgeu -- OK")
      // rs1 = 20, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 20,
          value2 = 30,
          function3 = 7,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))

      When("bgeu -- OK")
      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 7,
          immediateOrFunction7 = 200,
          opcode = 99
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))

      When("lb")
      // rs1 = 40, rs = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 1000,
          immediateOrFunction7 = 30,
          opcode = 3
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("lh")
      // rs1 = 40, rs = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 1000,
          immediateOrFunction7 = 30,
          function3 = 1,
          opcode = 3
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("lw")
      // rs1 = 40, rs = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 1000,
          immediateOrFunction7 = 30,
          function3 = 2,
          opcode = 3
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("ld")
      // rs1 = 40, rs = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 1000,
          immediateOrFunction7 = 30,
          function3 = 3,
          opcode = 3
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("lbu")
      // rs1 = 40, rs = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          immediateOrFunction7 = 30,
          function3 = 4,
          opcode = 3
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("lhu")
      // rs1 = 40, rs = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          immediateOrFunction7 = 30,
          function3 = 5,
          opcode = 3
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("lwu")
      // rs1 = 40, rs = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          immediateOrFunction7 = 30,
          function3 = 6,
          opcode = 3
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sb")
      // rs1 = 40, rs = 30, offset = 200
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          immediateOrFunction7 = 200,
          opcode = 35
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values =
        Some(ExecutorValue(destinationTag = 10, value = 240))
      )
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sh")
      // rs1 = 40, rs = 30, offset = 200
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 1,
          immediateOrFunction7 = 200,
          opcode = 35
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values =
        Some(ExecutorValue(destinationTag = 10, value = 240))
      )
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sw")
      // rs1 = 40, rs = 30, offset = 200
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 2,
          immediateOrFunction7 = 200,
          opcode = 35
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values =
        Some(ExecutorValue(destinationTag = 10, value = 240))
      )
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sd")
      // rs1 = 40, rs = 30, offset = 200
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 3,
          immediateOrFunction7 = 200,
          opcode = 35
        )
      )
      c.expectout(values = None)
      c.expectLSQ(values =
        Some(ExecutorValue(destinationTag = 10, value = 240))
      )
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("addi")
      // rs1 = 40, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          immediateOrFunction7 = 30,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("addw")
      // rs1 = 0xFFFF_FFFF, rs2 = 10 オーバーフローして 9
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 0xffff_ffffL,
          value2 = 10,
          opcode = 59
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 9)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("addw -- negative")
      // rs1 = 5, rs2 = -10 オーバーフローして -5
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 5,
          value2 = -10,
          opcode = 59
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = -5)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("addiw")
      // rs1 = 40, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          immediateOrFunction7 = 30,
          opcode = 27
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("slti -- NG")
      // rs1 = 40, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          immediateOrFunction7 = 30,
          function3 = 2,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("slti -- OK")
      // rs1 = 20, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 20,
          immediateOrFunction7 = 30,
          function3 = 2,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sltiu -- NG")
      // rs1 = 40, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          immediateOrFunction7 = 30,
          function3 = 3,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sltiu -- OK")
      // rs1 = 20, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 20,
          immediateOrFunction7 = 30,
          function3 = 3,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("xori")
      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 24(b11000)
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          immediateOrFunction7 = 18,
          function3 = 4,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 24)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("ori")
      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 26(b11010)
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          immediateOrFunction7 = 18,
          function3 = 6,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 26)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("andi")
      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 2(b00010)
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          immediateOrFunction7 = 18,
          function3 = 7,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 2)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("slli")
      // rs1 = 10, rs2 = 2, rd = 40
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          immediateOrFunction7 = 2,
          function3 = 1,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 40)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("srli")
      // rs1 = 64(b100 0000), rs2 = 3, rd = 8
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 64,
          immediateOrFunction7 = 3,
          function3 = 5,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 8)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("srai")
      // rs1 = 7(b0111), rs2 = 2, rd = 1(b0001)
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 7,
          immediateOrFunction7 = 2 + 2048,
          function3 = 5,
          opcode = 19
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("srai -- negative")
      // rs1 = -123, rs2 = 2, rd = -31
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = -123,
          function3 = 5,
          immediateOrFunction7 = 2 + 1024,
          opcode = 19
        )
      )
      c.expectout(values =
        Some(ExecutorValue(destinationTag = 10, value = -31))
      )
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("add")
      // rs1 = 40, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sub")
      // rs1 = 40, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          immediateOrFunction7 = 1024,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 10)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sll")
      // rs1 = 10, rs2 = 2, rd = 40
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          value2 = 2,
          function3 = 1,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 40)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("slt -- NG")
      // rs1 = 40, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 2,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("slt -- OK")
      // rs1 = 20, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 20,
          value2 = 30,
          function3 = 2,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sltu --NG")
      // rs1 = 40, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          function3 = 3,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sltu -- OK")
      // rs1 = 20, rs2 = 30
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 20,
          value2 = 30,
          function3 = 3,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("xor")
      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 24(b11000)
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          value2 = 18,
          function3 = 4,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 24)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("srl")
      // rs1 = 64, rs2 = 3, rd = 8
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 64,
          value2 = 3,
          function3 = 5,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 8)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sra")
      // rs1 = 10(1010), rs2 = 2, rd = 2
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          value2 = 2,
          function3 = 5,
          immediateOrFunction7 = 32,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 2)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("sra -- negative")
      // rs1 = -100, rs2 = 2, rd = -25
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = -100,
          value2 = 2,
          function3 = 5,
          immediateOrFunction7 = 1024,
          opcode = 51
        )
      )
      c.expectout(values =
        Some(ExecutorValue(destinationTag = 10, value = -25))
      )
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("or")
      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 26(b11010)
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          value2 = 18,
          function3 = 6,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 26)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))

      When("and")
      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 2(b00010)
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 10,
          value2 = 18,
          function3 = 7,
          opcode = 51
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 2)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
    }
  }

  it should "be compatible with C extension" in {
    test(new ExecutorWrapper()) { c =>
      c.io.fetch.ready.poke(true)
      c.io.out.ready.poke(true)

      When("beq -- NG")
      // rs1 = 40, rs = 30, offset = 200
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 30,
          immediateOrFunction7 = 200,
          opcode = 99,
          wasCompressed = true
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 2))

      When("beq -- OK")
      // rs1 = 40, rs = 40, offset = 200
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 40,
          value2 = 40,
          immediateOrFunction7 = 200,
          opcode = 99,
          wasCompressed = true
        )
      )
      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))

      When("jalr")
      // rs1 = 40, rs2(extend_offset) = 16
      c.setALU(values =
        ReservationValue(
          destinationTag = 10,
          value1 = 104,
          value2 = 100,
          immediateOrFunction7 = 16,
          opcode = 103,
          wasCompressed = true
        )
      )
      c.expectout(values =
        Some(ExecutorValue(destinationTag = 10, value = 102))
      )
      c.expectLSQ(None)
      c.expectFetch(values = FetchValue(valid = true, programCounter = 20))
    }

  }
}
