package b4processor.modules.executor

import b4processor.Parameters
import b4processor.connections.BranchOutput
import b4processor.utils.operations.ALUOperation
import b4processor.utils.{ExecutorValue, FetchValue, ReservationValue, Tag}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util.Irrevocable
import org.scalatest.GivenWhenThen

import scala.math.pow

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
  executor.io.reservationStation.bits.operation := io.reservationStation.bits.operation
  executor.io.reservationStation.bits.wasCompressed := io.reservationStation.bits.wasCompressed
  executor.io.reservationStation.bits.branchOffset := io.reservationStation.bits.branchOffset
  executor.io.reservationStation.valid := io.reservationStation.valid
  executor.io.out.ready := io.out.ready
  io.reservationStation.ready := executor.io.reservationStation.ready

  io.out.value := executor.io.out.bits.value.asSInt
  io.out.valid := executor.io.out.valid
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
    reservationstation.bits.operation.poke(values.operation)
  }

  def expectout(
    values: Option[ExecutorValue],
    valid: Boolean = true,
    message: String = "",
  ): Unit = {
    val out = this.io.out
    out.valid.expect(valid, message)
    if (values.isDefined) {
      out.destinationTag.expect(Tag(0, values.get.destinationTag), message)
      out.value.expect(values.get.value, message)
    }
  }

  def expectLSQ(values: Option[ExecutorValue], valid: Boolean = true): Unit = {
    val out = this.io.out
    out.valid.expect(valid)
    if (values.isDefined) {
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

      When("ADD")
      for (_ <- 0 until 100) {
        val reg = (math.random() * pow(2, defaultParams.tagWidth)).toInt
        val a = (math.random() * pow(2, 64)).toLong
        val b = (math.random() * pow(2, 64)).toLong
        c.setALU(
          ReservationValue(
            destinationTag = reg,
            value1 = a,
            value2 = b,
            operation = ALUOperation.Add,
          ),
        )
        c.expectout(values =
          Some(ExecutorValue(destinationTag = reg, value = a + b)),
        )
        c.expectLSQ(None)
        c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      }

      When("ADDW")
      for (_ <- 0 until 100) {
        val reg = (math.random() * pow(2, defaultParams.tagWidth)).toInt
        val a = (math.random() * pow(2, 64)).toLong
        val b = (math.random() * pow(2, 64)).toLong
        c.setALU(
          ReservationValue(
            destinationTag = reg,
            value1 = a,
            value2 = b,
            operation = ALUOperation.AddW,
          ),
        )
        c.expectout(values =
          Some(
            ExecutorValue(destinationTag = reg, value = (a + b).toInt.toLong),
          ),
        )
        c.expectLSQ(None)
        c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      }

      When("SUB")
      for (_ <- 0 until 100) {
        val reg = (math.random() * pow(2, defaultParams.tagWidth)).toInt
        val a = (math.random() * pow(2, 64)).toLong
        val b = (math.random() * pow(2, 64)).toLong
        c.setALU(
          ReservationValue(
            destinationTag = reg,
            value1 = a,
            value2 = b,
            operation = ALUOperation.Sub,
          ),
        )
        c.expectout(values =
          Some(ExecutorValue(destinationTag = reg, value = a - b)),
        )
        c.expectLSQ(None)
        c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      }

      When("SUBW")
      for (_ <- 0 until 100) {
        val reg = (math.random() * pow(2, defaultParams.tagWidth)).toInt
        val a = (math.random() * pow(2, 64)).toLong
        val b = (math.random() * pow(2, 64)).toLong
        c.setALU(
          ReservationValue(
            destinationTag = reg,
            value1 = a,
            value2 = b,
            operation = ALUOperation.SubW,
          ),
        )
        c.expectout(values =
          Some(
            ExecutorValue(destinationTag = reg, value = (a - b).toInt.toLong),
          ),
        )
        c.expectLSQ(None)
        c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      }

      When("SLL")
      for (_ <- 0 until 100) {
        val reg = (math.random() * pow(2, defaultParams.tagWidth)).toInt
        val a = ((math.random() * 2 - 1) * pow(2, 64)).toLong
        val b = ((math.random() * 2 - 1) * pow(2, 6)).toLong
        c.setALU(
          ReservationValue(
            destinationTag = reg,
            value1 = a,
            value2 = b,
            operation = ALUOperation.Sll,
          ),
        )
        c.expectout(values =
          Some(ExecutorValue(destinationTag = reg, value = a << b)),
        )
        c.expectLSQ(None)
        c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      }

      When("SRA")
      for (_ <- 0 until 100) {
        val reg = (math.random() * pow(2, defaultParams.tagWidth)).toInt
        val a = ((math.random() * 2 - 1) * pow(2, 64)).toLong
        val b = ((math.random() * 2 - 1) * pow(2, 6)).toLong
        c.setALU(
          ReservationValue(
            destinationTag = reg,
            value1 = a,
            value2 = b,
            operation = ALUOperation.Sra,
          ),
        )
        c.expectout(values =
          Some(ExecutorValue(destinationTag = reg, value = a >> b)),
        )
        c.expectLSQ(None)
        c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      }

      When("SRL")
      for (_ <- 0 until 100) {
        val reg = (math.random() * pow(2, defaultParams.tagWidth)).toInt
        val a = ((math.random() * 2 - 1) * pow(2, 64)).toLong
        val b = ((math.random() * 2 - 1) * pow(2, 6)).toLong
        c.setALU(
          ReservationValue(
            destinationTag = reg,
            value1 = a,
            value2 = b,
            operation = ALUOperation.Srl,
          ),
        )
        c.expectout(values =
          Some(ExecutorValue(destinationTag = reg, value = a >>> b)),
        )
        c.expectLSQ(None)
        c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      }

//      When("SRLW")
      //      for (_ <- 0 until 100) {
      //        val reg = (math.random() * pow(2, defaultParams.tagWidth)).toInt
      //        val a = ((math.random() - 0.5) * pow(2, 64)).toLong
      //        val b = (math.random() * pow(2, 5)).toLong
      //        c.setALU(
      //          ReservationValue(
      //            destinationTag = reg,
      //            value1 = a,
      //            value2 = b,
      //            operation = ALUOperation.SrlW
      //          )
      //        )
      //        println(a, b,(a.toInt >>> b).toLong)
      //        c.expectout(values =
      //          Some(
      //            ExecutorValue(destinationTag = reg, value = (a.toInt >>> b).toLong)
      //          )
      //        )
      //        c.expectLSQ(None)
      //        c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //      }

      //      When("jal")
      //      // rs1 = 40, rs2 = 16
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40, // imm
      //          value2 = 100, // PC
      //          opcode = 111
      //        )
      //      )
      //      c.expectout(values =
      //        Some(ExecutorValue(destinationTag = 10, value = 104))
      //      )
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("jalr")
      //      // rs1 = 40, rs2(extend_offset) = 16
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 104,
      //          value2 = 100,
      //          immediateOrFunction7 = 16,
      //          opcode = 103
      //        )
      //      )
      //      c.expectout(values =
      //        Some(ExecutorValue(destinationTag = 10, value = 104))
      //      )
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 20))
      //
      //      When("beq -- NG")
      //      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))
      //
      //      When("beq -- OK")
      //      // rs1 = 40, rs = 40, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 40,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))
      //
      //      When("bne -- NG")
      //      // rs1 = 40, rs = 40, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 40,
      //          function3 = 1,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))
      //
      //      When("bne -- OK")
      //      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 1,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))
      //
      //      When("blt -- NG")
      //      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 4,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))
      //
      //      When("blt -- OK")
      //      // rs1 = 20, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 20,
      //          value2 = 30,
      //          function3 = 4,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))
      //
      //      When("bge -- NG")
      //      // rs1 = 20, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 20,
      //          value2 = 30,
      //          function3 = 5,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))
      //
      //      When("bge -- OK")
      //      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 5,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))
      //
      //      When("bltu -- NG")
      //      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 6,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))
      //
      //      When("bltu -- OK")
      //      // rs1 = 20, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 20,
      //          value2 = 30,
      //          function3 = 6,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))
      //
      //      When("bgeu -- OK")
      //      // rs1 = 20, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 20,
      //          value2 = 30,
      //          function3 = 7,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 4))
      //
      //      When("bgeu -- OK")
      //      // rs1 = 40, rs = 30, offset = 200 (jump先： PC + (offset*2))
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 7,
      //          immediateOrFunction7 = 200,
      //          opcode = 99
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))
      //
      //      When("lb")
      //      // rs1 = 40, rs = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 1000,
      //          immediateOrFunction7 = 30,
      //          opcode = 3
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("lh")
      //      // rs1 = 40, rs = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 1000,
      //          immediateOrFunction7 = 30,
      //          function3 = 1,
      //          opcode = 3
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("lw")
      //      // rs1 = 40, rs = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 1000,
      //          immediateOrFunction7 = 30,
      //          function3 = 2,
      //          opcode = 3
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("ld")
      //      // rs1 = 40, rs = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 1000,
      //          immediateOrFunction7 = 30,
      //          function3 = 3,
      //          opcode = 3
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("lbu")
      //      // rs1 = 40, rs = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          immediateOrFunction7 = 30,
      //          function3 = 4,
      //          opcode = 3
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("lhu")
      //      // rs1 = 40, rs = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          immediateOrFunction7 = 30,
      //          function3 = 5,
      //          opcode = 3
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("lwu")
      //      // rs1 = 40, rs = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          immediateOrFunction7 = 30,
      //          function3 = 6,
      //          opcode = 3
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sb")
      //      // rs1 = 40, rs = 30, offset = 200
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          immediateOrFunction7 = 200,
      //          opcode = 35
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values =
      //        Some(ExecutorValue(destinationTag = 10, value = 240))
      //      )
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sh")
      //      // rs1 = 40, rs = 30, offset = 200
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 1,
      //          immediateOrFunction7 = 200,
      //          opcode = 35
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values =
      //        Some(ExecutorValue(destinationTag = 10, value = 240))
      //      )
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sw")
      //      // rs1 = 40, rs = 30, offset = 200
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 2,
      //          immediateOrFunction7 = 200,
      //          opcode = 35
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values =
      //        Some(ExecutorValue(destinationTag = 10, value = 240))
      //      )
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sd")
      //      // rs1 = 40, rs = 30, offset = 200
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 3,
      //          immediateOrFunction7 = 200,
      //          opcode = 35
      //        )
      //      )
      //      c.expectout(values = None)
      //      c.expectLSQ(values =
      //        Some(ExecutorValue(destinationTag = 10, value = 240))
      //      )
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("addi")
      //      // rs1 = 40, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          immediateOrFunction7 = 30,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("addw")
      //      // rs1 = 0xFFFF_FFFF, rs2 = 10 オーバーフローして 9
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 0xffff_ffffL,
      //          value2 = 10,
      //          opcode = 59
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 9)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("addw -- negative")
      //      // rs1 = 5, rs2 = -10 オーバーフローして -5
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 5,
      //          value2 = -10,
      //          opcode = 59
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = -5)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("addiw")
      //      // rs1 = 40, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          immediateOrFunction7 = 30,
      //          opcode = 27
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("slti -- NG")
      //      // rs1 = 40, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          immediateOrFunction7 = 30,
      //          function3 = 2,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("slti -- OK")
      //      // rs1 = 20, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 20,
      //          immediateOrFunction7 = 30,
      //          function3 = 2,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sltiu -- NG")
      //      // rs1 = 40, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          immediateOrFunction7 = 30,
      //          function3 = 3,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sltiu -- OK")
      //      // rs1 = 20, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 20,
      //          immediateOrFunction7 = 30,
      //          function3 = 3,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("xori")
      //      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 24(b11000)
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          immediateOrFunction7 = 18,
      //          function3 = 4,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 24)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("ori")
      //      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 26(b11010)
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          immediateOrFunction7 = 18,
      //          function3 = 6,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 26)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("andi")
      //      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 2(b00010)
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          immediateOrFunction7 = 18,
      //          function3 = 7,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 2)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("slli")
      //      // rs1 = 10, rs2 = 2, rd = 40
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          immediateOrFunction7 = 2,
      //          function3 = 1,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 40)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("srli")
      //      // rs1 = 64(b100 0000), rs2 = 3, rd = 8
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 64,
      //          immediateOrFunction7 = 3,
      //          function3 = 5,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 8)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("srai")
      //      // rs1 = 7(b0111), rs2 = 2, rd = 1(b0001)
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 7,
      //          immediateOrFunction7 = 2 + 2048,
      //          function3 = 5,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("srai -- negative")
      //      // rs1 = -123, rs2 = 2, rd = -31
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = -123,
      //          function3 = 5,
      //          immediateOrFunction7 = 2 + 1024,
      //          opcode = 19
      //        )
      //      )
      //      c.expectout(values =
      //        Some(ExecutorValue(destinationTag = 10, value = -31))
      //      )
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("add")
      //      // rs1 = 40, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 70)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sub")
      //      // rs1 = 40, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          immediateOrFunction7 = 1024,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 10)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sll")
      //      // rs1 = 10, rs2 = 2, rd = 40
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          value2 = 2,
      //          function3 = 1,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 40)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("slt -- NG")
      //      // rs1 = 40, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 2,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("slt -- OK")
      //      // rs1 = 20, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 20,
      //          value2 = 30,
      //          function3 = 2,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sltu --NG")
      //      // rs1 = 40, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          function3 = 3,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sltu -- OK")
      //      // rs1 = 20, rs2 = 30
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 20,
      //          value2 = 30,
      //          function3 = 3,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("xor")
      //      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 24(b11000)
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          value2 = 18,
      //          function3 = 4,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 24)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("srl")
      //      // rs1 = 64, rs2 = 3, rd = 8
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 64,
      //          value2 = 3,
      //          function3 = 5,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 8)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sra")
      //      // rs1 = 10(1010), rs2 = 2, rd = 2
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          value2 = 2,
      //          function3 = 5,
      //          immediateOrFunction7 = 32,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 2)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("sra -- negative")
      //      // rs1 = -100, rs2 = 2, rd = -25
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = -100,
      //          value2 = 2,
      //          function3 = 5,
      //          immediateOrFunction7 = 1024,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values =
      //        Some(ExecutorValue(destinationTag = 10, value = -25))
      //      )
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("or")
      //      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 26(b11010)
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          value2 = 18,
      //          function3 = 6,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 26)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //
      //      When("and")
      //      // rs1 = 10(b1010), rs2 = 18(b10010), rd = 2(b00010)
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 10,
      //          value2 = 18,
      //          function3 = 7,
      //          opcode = 51
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 2)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = false, programCounter = 0))
      //    }
      //  }
      //
      //  it should "be compatible with C extension" in {
      //    test(new ExecutorWrapper()) { c =>
      //      c.io.fetch.ready.poke(true)
      //      c.io.out.ready.poke(true)
      //
      //      When("beq -- NG")
      //      // rs1 = 40, rs = 30, offset = 200
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 30,
      //          immediateOrFunction7 = 200,
      //          opcode = 99,
      //          wasCompressed = true
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 0)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 2))
      //
      //      When("beq -- OK")
      //      // rs1 = 40, rs = 40, offset = 200
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 40,
      //          value2 = 40,
      //          immediateOrFunction7 = 200,
      //          opcode = 99,
      //          wasCompressed = true
      //        )
      //      )
      //      c.expectout(values = Some(ExecutorValue(destinationTag = 10, value = 1)))
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 400))
      //
      //      When("jalr")
      //      // rs1 = 40, rs2(extend_offset) = 16
      //      c.setALU(values =
      //        ReservationValue(
      //          destinationTag = 10,
      //          value1 = 104,
      //          value2 = 100,
      //          immediateOrFunction7 = 16,
      //          opcode = 103,
      //          wasCompressed = true
      //        )
      //      )
      //      c.expectout(values =
      //        Some(ExecutorValue(destinationTag = 10, value = 102))
      //      )
      //      c.expectLSQ(None)
      //      c.expectFetch(values = FetchValue(valid = true, programCounter = 20))
      //    }
      //
    }
  }
}
