package b4smt.modules.fetch

import b4smt.Parameters
import b4smt.connections.{Fetch2BranchPrediction, Fetch2FetchBuffer}
import b4smt.modules.branch_output_collector.CollectedBranchAddresses
import b4smt.modules.cache.{CacheFetchInterface, InstructionMemoryCache}
import b4smt.modules.fetch.{BranchType, Fetch}
import b4smt.modules.memory.ExternalMemoryInterface
import b4smt.utils.{InstructionUtil, SimpleAXIMemory}
import chisel3._
import chisel3.util._
import chiselformal.SymbiYosysFormal
import chisel3.simulator.scalatest.ChiselSim
import chisel3.simulator.PeekPokeAPI
import org.scalatest.flatspec.AnyFlatSpec

import scala.math

/** Fetch wrapper
  *
  * Contains fetch, cache, and instruction memory
  */
class FetchWrapper()(implicit params: Parameters)
    extends Module
    with PeekPokeAPI {
  val io = IO(new Bundle {

    /** Branch prediction */
    val prediction = Vec(params.decoderPerThread, new Fetch2BranchPrediction)

    /** Branch target values from the execution unit */
    val collectedBranchAddresses = Flipped(new CollectedBranchAddresses)

    /** Decoder */
    val decoders = new Fetch2FetchBuffer

    /** Whether the load-store queue is empty */
    val loadStoreQueueEmpty = Input(Bool())

    /** Whether the reorder buffer is empty */
    val reorderBufferEmpty = Input(Bool())

    /** Address requested from the cache */
    val cacheAddress = Decoupled(UInt(64.W))

    /** Output from the cache */
    val cacheOutput = Vec(params.decoderPerThread, Valid(UInt(64.W)))

    /** Program counter */
    val PC = Output(UInt(64.W))

    /** Program counter for the next clock cycle */
    val nextPC = Output(UInt(64.W))

    /** Branch type of each instruction */
    val branchTypes = Output(Vec(params.decoderPerThread, new BranchType.Type))

    val memorySetup = Flipped(Valid(UInt(64.W)))

    val interrupt = Input(Bool())
    val isError = Input(Bool())
  })

  val fetch = Module(new Fetch(1))
  val cache = Module(new InstructionMemoryCache())
  val cacheFetchIf = Module(new CacheFetchInterface)
  val memoryInterface = Module(new ExternalMemoryInterface)
  val axiMemory = Module(new SimpleAXIMemory)

  fetch.io.prediction <> io.prediction
  fetch.io.fetchBuffer <> io.decoders
  fetch.io.collectedBranchAddresses <> io.collectedBranchAddresses
  fetch.io.reorderBufferEmpty <> io.reorderBufferEmpty
  fetch.io.loadStoreQueueEmpty <> io.loadStoreQueueEmpty
  fetch.io.branchTypes.get <> io.branchTypes
  fetch.io.csr := DontCare
  fetch.io.csrReservationStationEmpty := true.B
  fetch.io.fetchBuffer.empty := true.B
  fetch.io.isError := io.isError
  fetch.io.threadId := 0.U
  fetch.io.interrupt := io.interrupt

  cacheFetchIf.io.cache <> cache.io.fetch
  cacheFetchIf.io.fetch <> fetch.io.cache

  cache.io.memory.request <> memoryInterface.io.instruction(0).request
  cache.io.memory.response <> memoryInterface.io.instruction(0).response
  cache.io.threadId := 0.U
  cache.io.flush := false.B

  (io.cacheOutput zip cacheFetchIf.io.fetch.perDecoder) foreach {
    case (out, value) =>
      out := value.response
  }

  memoryInterface.io.data.read.request.valid := false.B
  memoryInterface.io.data.read.request.bits := DontCare
  memoryInterface.io.data.write.request.valid := false.B
  memoryInterface.io.data.write.request.bits := DontCare
  memoryInterface.io.data.read.response.ready := true.B
  memoryInterface.io.data.write.requestData.bits := DontCare
  memoryInterface.io.data.write.requestData.valid := false.B
  memoryInterface.io.data.write.response.ready := true.B
  memoryInterface.io.amo.read.request.valid := false.B
  memoryInterface.io.amo.read.request.bits := DontCare
  memoryInterface.io.amo.write.request.valid := false.B
  memoryInterface.io.amo.write.request.bits := DontCare
  memoryInterface.io.amo.write.requestData.bits := DontCare
  memoryInterface.io.amo.write.requestData.valid := false.B
  memoryInterface.io.amo.read.response.ready := true.B
  memoryInterface.io.amo.write.response.ready := true.B

  axiMemory.axi <> memoryInterface.io.coordinator
  axiMemory.simulationSource.input <> io.memorySetup

  fetch.io.cache.requestNext <> io.cacheAddress
  fetch.io.cache.perDecoder.zip(io.cacheOutput) foreach { case (f, c) =>
    f.response := c
  }

  io.PC := fetch.io.PC.get
  io.nextPC := fetch.io.nextPC.get

  fetch.io.fetchBuffer.toBuffer.foreach(v => v.ready := true.B)

  /** Initialization */
  def initialize(memoryInit: => Seq[UInt]): Unit = {
    this.setPrediction(Seq.fill(params.decoderPerThread)(false))
    this.io.memorySetup.valid.poke(true)
    this.io.memorySetup.bits.poke(memoryInit.length)
    for (i <- memoryInit.indices) {
      this.clock.step()
      this.io.memorySetup.bits.poke(memoryInit(i))
    }
    this.clock.step()
    this.io.memorySetup.valid.poke(false)
    this.clock.step()

  }

  def waitForCacheValid(): Unit = {
    clock.stepUntil(io.cacheOutput(0).valid, 1, 200)
    io.cacheOutput(0).valid.expect(true)
  }

  /** Set predictions */
  def setPrediction(values: Seq[Boolean]): Unit = {
    for (i <- 0 until params.decoderPerThread) {
      this.io.prediction(i).prediction.poke(values(i))
    }
  }

  /** Set the branch target */
  def setExecutorBranchResult(results: Option[Int] = None): Unit = {
    io.collectedBranchAddresses.addresses.valid.poke(results.isDefined)
    io.collectedBranchAddresses.addresses.bits.programCounterOffset
      .poke(results.getOrElse(0).S)
  }
}

class FetchTest extends AnyFlatSpec with ChiselSim with SymbiYosysFormal {

  behavior of "Fetch"
  implicit val defaultParams: b4smt.Parameters =
    Parameters(
      debug = true,
      threads = 1,
      decoderPerThread = 2,
      instructionStart = 0x1000_0000,
    )

  // Check whether ordinary instructions and branches can be distinguished
  // Instructions to write
  // nop                  00000013
  // beq zero,zero,LABEL  00000463
  // nop                  00000013
  // LABEL:
  // TODO FIX
  ignore should "load memory" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil
          .fromStringSeq32bit(Seq("00000013", "00000463", "00000013")),
      )

      c.waitForCacheValid()
      c.io.cacheAddress.bits.expect(0x10000000)
      c.io.cacheAddress.bits.expect(0x10000004)

      c.io.cacheOutput(0).bits.expect("x00000013".U)
      c.io.cacheOutput(0).valid.expect(true)

      c.io.cacheOutput(1).bits.expect("x00000463".U)
      c.io.cacheOutput(1).valid.expect(true)
    }
  }

  // Both values can be read using prediction
  // Instructions to write
  // LOOP:
  // beq zero,zero,LOOP 00000063
  // TODO FIX
  ignore should "read both values in loop" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(Seq("00000063", "00000000")),
      )
      c.setPrediction(Seq(true, true))

      c.waitForCacheValid()
      c.io.cacheAddress.bits.expect(0x10000000)
      c.io.cacheAddress.bits.expect(0x10000000)

      c.io.cacheOutput(0).bits.expect("x00000063".U)
      c.io.cacheOutput(0).valid.expect(true)

      c.io.cacheOutput(1).bits.expect("x00000063".U)
      c.io.cacheOutput(1).valid.expect(true)

      c.clock.step()
    }
  }

  // Check whether ordinary instructions and branch targets are recognized
  // Instructions to write
  // nop                  00000013
  // beq zero,zero,LABEL  00000463
  // nop                  00000013
  // LABEL:
  it should "understand branch prediction=false" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil
          .fromStringSeq32bit(Seq("00000013", "00000463", "00000013")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)
    }
  }

  // Check whether ordinary instructions and branch targets are recognized
  // Instructions to write
  // nop                  00000013
  // beq zero,zero,LABEL  00000463
  // nop                  00000013
  // LABEL:
  it should "understand branch prediction=true" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil
          .fromStringSeq32bit(Seq("00000013", "00000463", "00000013")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)
    }
  }

  // Check whether a loop returns to its own address
  // Instructions to write
  // nop                00000013
  // LOOP:
  // beq zero,zero,LOOP 00000063
  it should "understand loop to self" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(Seq("00000013", "00000063")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)

      c.clock.step()

      c.setExecutorBranchResult(Some(0x10000004))

      c.io.nextPC.expect(0x10000004)
    }
  }

  // Return to the same instruction in a loop
  // Instructions to write
  // LOOP:
  // nop                00000013
  // beq zero,zero,LOOP 00000063
  it should "understand loop" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(Seq("00000013", "00000063")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)
      c.clock.step()

      c.setExecutorBranchResult(Some(0x00000000))
      c.io.nextPC.expect(0x10000004)
      c.clock.step()

      c.io.nextPC.expect(0x10000004)
      c.clock.step()
    }
  }

  // Infinite loop using jal
  // Instructions to write
  // LOOP:
  // nop                00000013
  // j                  ffdff06f
  it should "understand jal loop" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(Seq("00000013", "ffdff06f")),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000000)
    }
  }

  // Unconditionally change the address using jal
  // Instructions to write
  // START:
  // nop      00000013
  // j LABEL  0180006f
  // nop      00000013
  // nop      00000013
  // nop      00000013
  // nop      00000013
  // nop      00000013
  // LABEL:
  // nop      00000013
  // j START  fe1ff06f
  // TODO FIX
  ignore should "understand jal jumps" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(
          Seq(
            "00000013",
            "0180006f",
            "00000013",
            "00000013",
            "00000013",
            "00000013",
            "00000013",
            "00000013",
            "fe1ff06f",
          ),
        ),
      )
      c.waitForCacheValid()
      c.io.nextPC.expect(0x1000001c)

      c.clock.step()
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000020)

      c.clock.step()
      c.waitForCacheValid()
      c.io.nextPC.expect(0x10000004)
    }
  }

  // Use Fence
  // Instructions to write
  // nop    00000013
  // fence  0ff0000f
  // nop    00000013
  // nop    00000013
  it should "understand fence" in {
    simulate(
      new FetchWrapper(
      ),
    ) { c =>
      c.initialize(
        InstructionUtil.fromStringSeq32bit(
          Seq("00000013", "0ff0000f", "00000013", "00000013"),
        ),
      )
      c.waitForCacheValid()
      c.io.branchTypes(0).expect(BranchType.Next4)
      c.io.branchTypes(1).expect(BranchType.Fence)
      c.io.decoders.toBuffer(0).valid.expect(true)
      c.io.decoders.toBuffer(1).valid.expect(false)
      c.io.nextPC.expect(0x10000004)

      c.clock.step()
      c.io.nextPC.expect(0x10000004)
      c.io.decoders.toBuffer(0).valid.expect(false)
      c.io.decoders.toBuffer(1).valid.expect(false)

      c.clock.step()
      c.io.reorderBufferEmpty.poke(true)
      c.io.nextPC.expect(0x10000004)
      c.io.decoders.toBuffer(0).valid.expect(false)
      c.io.decoders.toBuffer(1).valid.expect(false)

      c.clock.step()
      c.io.loadStoreQueueEmpty.poke(true)
      c.io.nextPC.expect(0x10000004)
      c.io.decoders.toBuffer(0).valid.expect(false)
      c.io.decoders.toBuffer(1).valid.expect(false)

      c.clock.step()
      c.io.nextPC.expect(0x10000010)
      c.io.decoders.toBuffer(0).valid.expect(true)
      c.io.decoders.toBuffer(1).valid.expect(true)
    }
  }

  // TODO maybe not fix?
  // this takes too long
  ignore should "check formal" in {
    symbiYosysCheck(new FetchWrapper())
  }
}
