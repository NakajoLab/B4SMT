package b4smt.utils

import b4smt.{B4SMTCore, Parameters}
import chisel3._
import chisel3.simulator.PeekPokeAPI
import chisel3.util._
import _root_.circt.stage.ChiselStage

class B4SMTCoreWithMemory()(implicit params: Parameters)
    extends Module
    with PeekPokeAPI {
  val io = IO(new Bundle {
    val simulation = Flipped(Valid(UInt(64.W)))
    val registerFileContents =
      if (params.debug) Some(Output(Vec(params.threads, Vec(32, UInt(64.W)))))
      else None
    val accessMemoryAddress = new Bundle {
      val readAddress = Valid(UInt(64.W))
      val readData = Valid(UInt(64.W))
      val writeAddress = Valid(UInt(64.W))
      val writeData = Valid(UInt(64.W))
    }
    val simulationIO = new Bundle {
      val input = Flipped(Decoupled(UInt(8.W)))
      val output = Decoupled(UInt(8.W))
    }
  })
  val core = Module(new B4SMTCore)
  val axiMemory = Module(new SimpleAXIMemoryWithSimulationIO(1024 * 1024 * 512))
  core.axi <> axiMemory.axi
  io.simulation <> axiMemory.simulationSource.input
  axiMemory.simulationIO <> io.simulationIO
  if (params.debug)
    io.registerFileContents.get <> core.registerFileContents.get
  io.accessMemoryAddress.readAddress.valid := core.axi.readAddress.valid
  io.accessMemoryAddress.readAddress.bits := core.axi.readAddress.bits.ADDR
  io.accessMemoryAddress.readData.valid := core.axi.read.valid
  io.accessMemoryAddress.readData.bits := core.axi.read.bits.DATA
  io.accessMemoryAddress.writeAddress.valid := core.axi.writeAddress.valid
  io.accessMemoryAddress.writeAddress.bits := core.axi.writeAddress.bits.ADDR
  io.accessMemoryAddress.writeData.valid := core.axi.write.valid
  io.accessMemoryAddress.writeData.bits := core.axi.write.bits.DATA

  private def loadMemory(memoryInit: Seq[UInt]): Unit = {
    // simulate has already reset the core and memory loader. Keep reset low:
    // the loader uses registers and blocks AXI reads until the image is ready.
    io.simulationIO.input.valid.poke(false)
    io.simulationIO.input.bits.poke(0)
    io.simulationIO.output.ready.poke(false)
    io.simulation.valid.poke(true)
    io.simulation.bits.poke(memoryInit.length)
    for (word <- memoryInit) {
      clock.step()
      io.simulation.bits.poke(word)
    }
    for (_ <- 0 until 20) {
      clock.step()
      io.simulation.bits.poke(0)
    }
    clock.step()
    io.simulation.valid.poke(false)
    clock.step()
  }

  def initialize(instructions: String, binary: Boolean = false): Unit =
    loadMemory(
      if (binary) InstructionUtil.fromBinaryFile(instructions)
      else InstructionUtil.fromFile8bit(instructions + ".hex"),
    )

  def initialize64(instructions: String, binary: Boolean = false): Unit =
    loadMemory(
      if (binary) InstructionUtil.fromBinaryFile(instructions)
      else InstructionUtil.fromFile64bit(instructions + ".64.hex"),
    )

  def checkForWrite(address: UInt, value: UInt, timeout: Int = 500): Unit = {
    clock.stepUntil(io.accessMemoryAddress.writeAddress.valid, 1, timeout)
    io.accessMemoryAddress.writeAddress.valid.expect(true)
    io.accessMemoryAddress.writeAddress.bits.expect(address)
    clock.stepUntil(io.accessMemoryAddress.writeData.valid, 1, timeout)
    io.accessMemoryAddress.writeData.valid.expect(true)
    io.accessMemoryAddress.writeData.bits.expect(value)
    clock.step(10)
  }

  def checkForRegister(
    regNum: Int,
    value: BigInt,
    timeout: Int = 500,
    thread: Int = 0,
  ): Unit = {
    // Chisel 7.16's stepUntil compares wide sentinels with an incorrect byte count.
    val register = io.registerFileContents.get(thread)(regNum)
    var cycles = 0
    while (register.peek().litValue != value && cycles < timeout) {
      clock.step()
      cycles += 1
    }
    io.registerFileContents
      .get(thread)(regNum)
      .expect(
        value,
        s"thread $thread x$regNum did not reach $value within $timeout cycles",
      )
  }

  def checkForOutput(
    value: Char,
    timeout: Int = 500,
    print_value: Boolean = false,
  ): Unit = {
    clock.stepUntil(io.simulationIO.output.valid, 1, timeout)
    io.simulationIO.output.valid.expect(true)
    io.simulationIO.output.bits.expect(value.toInt)
    getOutput(timeout, print_value)
  }

  def checkForOutputAny(
    timeout: Int = 500,
    print_value: Boolean = false,
  ): Unit = {
    getOutput(timeout, print_value)
  }

  def checkForOutputString(
    str: String,
    char_timeout: Int = 500,
    print_value: Boolean = false,
  ): Unit = {
    var idx = 0
    var characters = 0L
    val maxCharacters = math.max(1L, str.length.toLong) * char_timeout
    while (idx < str.length && characters < maxCharacters) {
      val c = getOutput(timeout = char_timeout, print_value = print_value)
      characters += 1
      if (c == str(idx)) idx += 1 else idx = 0
    }
    assert(
      idx == str.length,
      s"output did not contain '$str' within $maxCharacters characters",
    )
  }

  def getOutput(timeout: Int = 500, print_value: Boolean = false): Char = {
    clock.stepUntil(io.simulationIO.output.valid, 1, timeout)
    io.simulationIO.output.valid.expect(true)
    io.simulationIO.output.ready.poke(true)
    val c = io.simulationIO.output.bits.peek().litValue.toChar
    if (print_value) print(c)
    clock.step()
    io.simulationIO.output.ready.poke(false)
    c
  }

  def checkForRegisterChange(
    regNum: Int,
    value: BigInt,
    timeout: Int = 500,
    thread: Int = 0,
  ): Unit = {
    var cycles = 0
    while (
      io.registerFileContents
        .get(thread)(regNum)
        .peek()
        .litValue == 0 && cycles < timeout
    ) {
      clock.step()
      cycles += 1
    }
    assert(
      io.registerFileContents.get(thread)(regNum).peek().litValue != 0,
      s"x$regNum did not change within $timeout cycles",
    )
    io.registerFileContents.get(thread)(regNum).expect(value)
    clock.step(3)
  }
}

object B4SMTCoreWithMemory extends App {
  implicit val params: b4smt.Parameters = Parameters(
    threads = 1,
    decoderPerThread = 2,
    maxRegisterFileCommitCount = 4,
    tagWidth = 5,
    loadStoreQueueIndexWidth = 3,
  )
  ChiselStage.emitSystemVerilogFile(new B4SMTCoreWithMemory())
}
