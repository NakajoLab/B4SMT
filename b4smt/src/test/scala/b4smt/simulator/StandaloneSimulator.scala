package b4smt.simulator

import b4smt.Parameters
import b4smt.utils.B4SMTCoreWithMemory
import chisel3._
import _root_.circt.stage.ChiselStage
import java.nio.file.{Files, Paths}

class StandaloneSimulator(implicit params: Parameters) extends Module {
  val loadValid = IO(Input(Bool()))
  val loadData = IO(Input(UInt(64.W)))
  val threadIndex = IO(Input(UInt(32.W)))
  val threadCount = IO(Output(UInt(32.W)))
  val programCapacity = IO(Output(UInt(64.W)))
  val registerIndex = IO(Input(UInt(5.W)))
  val registerValue = IO(Output(UInt(64.W)))
  val uartReady = IO(Input(Bool()))
  val uartValid = IO(Output(Bool()))
  val writeAddressValid = IO(Output(Bool()))
  val writeAddress = IO(Output(UInt(64.W)))
  val writeDataValid = IO(Output(Bool()))
  val writeData = IO(Output(UInt(64.W)))
  val uartData = IO(Output(UInt(8.W)))

  private val fixture = Module(new B4SMTCoreWithMemory())
  fixture.io.simulation.valid := loadValid
  fixture.io.simulation.bits := loadData
  fixture.io.simulationIO.input.valid := false.B
  fixture.io.simulationIO.input.bits := 0.U
  fixture.io.simulationIO.output.ready := uartReady
  writeAddressValid := fixture.io.accessMemoryAddress.writeAddress.valid
  writeAddress := fixture.io.accessMemoryAddress.writeAddress.bits
  writeDataValid := fixture.io.accessMemoryAddress.writeData.valid
  writeData := fixture.io.accessMemoryAddress.writeData.bits
  uartValid := fixture.io.simulationIO.output.valid
  uartData := fixture.io.simulationIO.output.bits
  threadCount := params.threads.U
  programCapacity := (512L * 1024 * 1024 - params.instructionStart % (512L * 1024 * 1024)).U
  private val threadRegisters =
    if (params.threads == 1) fixture.io.registerFileContents.get(0)
    else
      fixture.io.registerFileContents.get(
        threadIndex(chisel3.util.log2Ceil(params.threads) - 1, 0),
      )
  registerValue := threadRegisters(registerIndex)
}

object StandaloneSimulator {
  // ujson is already available through the existing Chisel dependencies.
  def readConfig(path: String): Parameters = {
    val json = ujson.read(Files.readString(Paths.get(path)))
    require(json.isInstanceOf[ujson.Obj], "configuration must be a JSON object")
    val fields = json.obj
    val defaults = Parameters()
    val names = defaults.productElementNames.toSet
    require(
      fields.keySet.subsetOf(names),
      s"unknown configuration fields: ${(fields.keySet -- names).mkString(", ")}",
    )

    def int(name: String, default: Int): Int = fields.get(name) match {
      case None => default
      case Some(ujson.Num(n)) if n.isWhole && n > 0 && n <= Int.MaxValue =>
        n.toInt
      case _ =>
        throw new IllegalArgumentException(s"$name must be a positive integer")
    }
    def bool(name: String, default: Boolean): Boolean = fields.get(name) match {
      case None                => default
      case Some(ujson.Bool(b)) => b
      case _ => throw new IllegalArgumentException(s"$name must be a boolean")
    }
    val instructionStart = fields.get("instructionStart") match {
      case None => defaults.instructionStart
      case Some(ujson.Num(n))
          if n.isWhole && n >= 0 && n <= 9007199254740991L =>
        n.toLong
      case Some(ujson.Str(s)) if s.matches("0[xX][0-9a-fA-F]+") =>
        java.lang.Long.parseLong(s.drop(2), 16)
      case _ =>
        throw new IllegalArgumentException(
          "instructionStart must be an exact nonnegative integer (<=2^53-1) or hex string",
        )
    }
    require(
      instructionStart % 8 == 0 && instructionStart <= Long.MaxValue - 512L * 1024 * 1024,
      "instructionStart must be 8-byte aligned and leave room for fixture memory",
    )
    val p = Parameters(
      tagWidth = int("tagWidth", defaults.tagWidth),
      executors = int("executors", defaults.executors),
      mulDivExecutors = int("mulDivExecutors", defaults.mulDivExecutors),
      loadStoreQueueIndexWidth =
        int("loadStoreQueueIndexWidth", defaults.loadStoreQueueIndexWidth),
      loadStoreQueueCheckLength =
        int("loadStoreQueueCheckLength", defaults.loadStoreQueueCheckLength),
      decoderPerThread = int("decoderPerThread", defaults.decoderPerThread),
      threads = int("threads", defaults.threads),
      maxRegisterFileCommitCount =
        int("maxRegisterFileCommitCount", defaults.maxRegisterFileCommitCount),
      maxDataMemoryCommitCount =
        int("maxDataMemoryCommitCount", defaults.maxDataMemoryCommitCount),
      fetchWidth = int("fetchWidth", defaults.fetchWidth),
      branchPredictionWidth =
        int("branchPredictionWidth", defaults.branchPredictionWidth),
      parallelOutput = int("parallelOutput", defaults.parallelOutput),
      instructionStart = instructionStart,
      debug = bool("debug", defaults.debug),
      enablePExt = bool("enablePExt", defaults.enablePExt),
      pextExecutors = int("pextExecutors", defaults.pextExecutors),
      reservationStationWidth =
        int("reservationStationWidth", defaults.reservationStationWidth),
      ICacheWay = int("ICacheWay", defaults.ICacheWay),
      ICacheSet = int("ICacheSet", defaults.ICacheSet),
      ICacheBlockWidth = int("ICacheBlockWidth", defaults.ICacheBlockWidth),
      ICacheDataNum = int("ICacheDataNum", defaults.ICacheDataNum),
      MemoryBurstLength = int("MemoryBurstLength", defaults.MemoryBurstLength),
    )
    require(p.debug, "debug=true is required for register checks")
    p
  }

  def config(params: Parameters): ujson.Obj =
    ujson.Obj.from(params.productElementNames.zip(params.productIterator).map {
      case (name, value: Boolean)            => name -> ujson.Bool(value)
      case ("instructionStart", value: Long) =>
        "instructionStart" -> ujson.Str("0x" + value.toHexString)
      case (name, value: Int) => name -> ujson.Num(value)
      case (name, value)      =>
        throw new IllegalArgumentException(
          s"unsupported parameter $name=$value",
        )
    })

  def main(args: Array[String]): Unit = {
    require(
      args.length == 4 && args(0) == "--config" && args(2) == "--target-dir",
      "usage: StandaloneSimulator --config PATH --target-dir DIR",
    )
    implicit val params: Parameters = readConfig(args(1))
    ChiselStage.emitSystemVerilogFile(
      new StandaloneSimulator,
      Array("--target-dir", args(3)),
      // Specialize every verification layer into the design, not optional binds.
      firtoolOpts = Array("--default-layer-specialization=enable"),
    )
  }
}
