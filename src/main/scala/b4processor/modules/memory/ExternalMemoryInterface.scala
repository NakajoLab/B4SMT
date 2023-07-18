package b4processor.modules.memory

import b4processor.Parameters
import b4processor.connections.OutputValue
import b4processor.structures.memoryAccess.MemoryAccessWidth
import b4processor.utils.{B4RRArbiter, FIFO, Tag}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4processor.utils.axi.{BurstSize, BurstType, ChiselAXI, Lock, Response}

class ExternalMemoryInterface(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val dataWriteRequests = Flipped(Irrevocable(new MemoryWriteTransaction))
    val dataReadRequests = Flipped(Irrevocable(new MemoryReadTransaction))
    val instructionFetchRequest =
      Vec(params.threads, Flipped(Irrevocable(new MemoryReadTransaction)))
    val dataReadOut = Irrevocable(new OutputValue)
    val dataWriteOut = Valid(new WriteResponse)
    val instructionOut = Vec(params.threads, Valid(new InstructionResponse))
    val coordinator = new ChiselAXI(64, 64)
  })

  // setDefaultOutputs
  locally {
    import io.coordinator._
    writeAddress.bits.BURST := BurstType.Incr
    writeResponse.ready := false.B
    write.valid := false.B
    readAddress.bits.USER := 0.U
    writeAddress.bits.USER := 0.U
    writeAddress.bits.PROT := 0.U
    readAddress.bits.CACHE := 0.U
    writeAddress.bits.ADDR := 0.U
    writeAddress.bits.LOCK := Lock.Normal
    write.bits.USER := 0.U
    writeAddress.bits.ID := 0.U
    readAddress.valid := false.B
    writeAddress.bits.CACHE := 0.U
    readAddress.bits.LEN := 0.U
    readAddress.bits.QOS := 0.U
    writeAddress.bits.SIZE := BurstSize.Size1
    write.bits.STRB := 0.U
    readAddress.bits.PROT := 0.U
    readAddress.bits.ADDR := 0.U
    readAddress.bits.LOCK := Lock.Normal
    readAddress.bits.SIZE := BurstSize.Size1
    read.ready := false.B
    write.bits.LAST := false.B
    readAddress.bits.ID := 0.U
    writeAddress.bits.QOS := 0.U
    writeAddress.bits.LEN := 0.U
    writeAddress.valid := false.B
    write.bits.DATA := 0.U
    readAddress.bits.BURST := BurstType.Incr
  }
  io.dataWriteRequests.ready := false.B
  io.dataReadOut.bits.value := 0.U
  io.dataReadOut.bits.isError := false.B
  io.dataReadOut.bits.tag := Tag(0, 0)
  io.dataReadRequests.ready := false.B
  io.dataWriteOut.valid := false.B
  io.dataWriteOut.bits := DontCare
  io.dataReadOut.valid := false.B
  for (tid <- 0 until params.threads) {
    io.instructionFetchRequest(tid).ready := false.B
    io.instructionOut(tid).valid := false.B
    io.instructionOut(tid).bits.inner := 0.U
  }

  // READ OPERATION -------------------------------------------
  val readQueue = Module(new FIFO(3)(new Bundle {
    val burstLength = UInt(8.W)
    val isInstruction = Bool()
    val tag = new Tag
    val size = new MemoryAccessWidth.Type()
    val offset = UInt(3.W)
    val signed = Bool()
  }))
  readQueue.output.ready := false.B
  readQueue.input.valid := false.B
  readQueue.input.bits := DontCare
  readQueue.flush := false.B

  private val instructionsArbiter = Module(
    new B4RRArbiter(new MemoryReadTransaction(), params.threads)
  )
  for (tid <- 0 until params.threads)
    instructionsArbiter.io.in(tid) <> io.instructionFetchRequest(tid)

  private val instructionOrReadDataArbiter = Module(
    new Arbiter(new MemoryReadTransaction(), 2)
  )
  instructionOrReadDataArbiter.io.in(0) <> instructionsArbiter.io.out
  instructionOrReadDataArbiter.io.in(1) <> io.dataReadRequests
  private val instructionOrReadDataQueue = Module(
    new FIFO(2)(new MemoryReadTransaction())
  )
  instructionOrReadDataQueue.input <> instructionOrReadDataArbiter.io.out
  instructionOrReadDataQueue.flush := false.B
  private val readTransaction = instructionOrReadDataQueue.output
  readTransaction.ready := false.B

  val readQueued = RegInit(false.B)
  val burstLen = RegInit(0.U(8.W))
  when(!readQueue.full) {
    when(readTransaction.valid) {
      locally {
        import io.coordinator.readAddress._
        valid := true.B
        bits.ADDR := readTransaction.bits.address(63, 3) ## 0.U(3.W)
        bits.LEN := readTransaction.bits.burstLength
        bits.SIZE := BurstSize.Size8
        bits.CACHE := "b0010".U
      }
      readQueue.input.valid := !readQueued
      readQueue.input.bits.burstLength := readTransaction.bits.burstLength
      readQueue.input.bits.isInstruction := readTransaction.bits.isInstruction
      readQueue.input.bits.tag := readTransaction.bits.outputTag
      readQueue.input.bits.offset := readTransaction.bits.address(2, 0)
      readQueue.input.bits.size := readTransaction.bits.size
      readQueue.input.bits.signed := readTransaction.bits.signed
      readQueued := true.B
      when(io.coordinator.readAddress.ready) {
        readTransaction.ready := true.B
        readQueued := false.B
      }
    }

  }

  when(!readQueue.empty) {
    when(readQueue.output.bits.isInstruction) {
      io.coordinator.read.ready := true.B
    }.otherwise {
      io.coordinator.read.ready := io.dataReadOut.ready
    }
    when(io.coordinator.read.valid) {
      when(io.coordinator.read.ready) {
        burstLen := burstLen + 1.U
        when(burstLen === readQueue.output.bits.burstLength) {
          readQueue.output.ready := true.B
          burstLen := 0.U
        }
      }
      when(readQueue.output.bits.isInstruction) {
        val tid = readQueue.output.bits.tag.threadId
        io.instructionOut(tid).valid := true.B
        val data = io.coordinator.read.bits.DATA
        io.instructionOut(tid).bits.inner := data
      }.otherwise {
        io.dataReadOut.valid := true.B
        io.dataReadOut.bits.tag := readQueue.output.bits.tag
        val data = io.coordinator.read.bits.DATA
        io.dataReadOut.bits.value := Mux1H(
          Seq(
            (readQueue.output.bits.size === MemoryAccessWidth.Byte) -> Mux1H(
              (0 until 8).map(i =>
                (readQueue.output.bits.offset === i.U) -> Cat(
                  Mux(
                    readQueue.output.bits.signed && data(i * 8 + 7),
                    "xFFFF_FFFF_FFFF_FF".U,
                    0.U
                  ),
                  data(i * 8 + 7, i * 8)
                )
              )
            ),
            (readQueue.output.bits.size === MemoryAccessWidth.HalfWord) -> Mux1H(
              Seq(0, 2, 4, 6).map(i =>
                (readQueue.output.bits.offset === i.U) -> Cat(
                  Mux(
                    readQueue.output.bits.signed && data(i * 8 + 15),
                    "xFFFF_FFFF_FFFF".U,
                    0.U
                  ),
                  data(i * 8 + 15, i * 8)
                )
              )
            ),
            (readQueue.output.bits.size === MemoryAccessWidth.Word) -> Mux1H(
              Seq(0, 4).map(i =>
                (readQueue.output.bits.offset === i.U) -> Cat(
                  Mux(
                    readQueue.output.bits.signed && data(i * 8 + 31),
                    "xFFFF_FFFF".U,
                    0.U
                  ),
                  data(i * 8 + 31, i * 8)
                )
              )
            ),
            (readQueue.output.bits.size === MemoryAccessWidth.DoubleWord) -> data
          )
        )
        io.dataReadOut.bits.isError := io.coordinator.read.bits.RESP =/= Response.Okay
      }
    }
  }

  // ----------------------------------------
  // WRITE OPERATION ------------------------
  // ----------------------------------------
  val dataWriteQueue = Module(new FIFO(3)(new Bundle {
    val data = UInt(64.W)
    val strb = UInt(8.W)
  }))
  dataWriteQueue.output.ready := false.B
  dataWriteQueue.input.valid := false.B
  dataWriteQueue.input.bits := DontCare
  dataWriteQueue.flush := false.B
  val writeResponseQueue = Module(new FIFO(3)(new Bundle {
    val tag = new Tag
  }))
  writeResponseQueue.output.ready := false.B
  writeResponseQueue.input.valid := false.B
  writeResponseQueue.input.bits := DontCare
  writeResponseQueue.flush := false.B

  val writeQueued = RegInit(false.B)
  when(io.dataWriteRequests.valid) {
    locally {
      import io.coordinator.writeAddress._
      valid := true.B
      bits.ADDR := io.dataWriteRequests.bits.address
      bits.LEN := 0.U
      bits.SIZE := BurstSize.Size8
      bits.BURST := BurstType.Incr
      bits.CACHE := "b0010".U

      dataWriteQueue.input.valid := !writeQueued
      dataWriteQueue.input.bits.data := io.dataWriteRequests.bits.data
      dataWriteQueue.input.bits.strb := io.dataWriteRequests.bits.mask
      writeResponseQueue.input.valid := !writeQueued
      writeResponseQueue.input.bits.tag := io.dataWriteRequests.bits.outputTag
      writeQueued := true.B
    }
    when(io.coordinator.writeAddress.ready) {
      io.dataWriteRequests.ready := true.B
      writeQueued := false.B
    }
  }

  when(!dataWriteQueue.empty) {
    io.coordinator.write.valid := true.B
    io.coordinator.write.bits.DATA := dataWriteQueue.output.bits.data
    io.coordinator.write.bits.STRB := dataWriteQueue.output.bits.strb
    io.coordinator.write.bits.LAST := true.B
    when(io.coordinator.write.ready) {
      dataWriteQueue.output.ready := true.B
    }
  }

  when(!writeResponseQueue.empty) {
    io.coordinator.writeResponse.ready := true.B
    when(io.coordinator.writeResponse.valid) {
      writeResponseQueue.output.ready := true.B
      io.dataWriteOut.valid := true.B
      io.dataWriteOut.bits.tag := writeResponseQueue.output.bits.tag
      io.dataWriteOut.bits.isError := false.B
    }
  }

}

object ExternalMemoryInterface extends App {
  implicit val params = {
    Parameters(decoderPerThread = 1, tagWidth = 4)
  }
  ChiselStage.emitSystemVerilogFile(new ExternalMemoryInterface)
}
