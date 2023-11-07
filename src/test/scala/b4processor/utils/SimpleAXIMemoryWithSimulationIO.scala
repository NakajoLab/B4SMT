package b4processor.utils

import _root_.circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.utils.axi.{ChiselAXI, Response}
import chisel3._
import chisel3.util._

class SimpleAXIMemoryWithSimulationIO(sizeBytes: Int = 1024 * 1024 * 16)(
  implicit params: Parameters,
) extends Module {
  val axi = IO(Flipped(new ChiselAXI(64, 64)))
  val simulationSource = IO(new Bundle {
    val input = Flipped(Valid(UInt(64.W)))
  })
  val simulationIO = IO(new Bundle {
    val input = Flipped(Decoupled(UInt(8.W)))
    val output = Decoupled(UInt(8.W))
  })

  private val memAddrMask = BitPat(
    "b00000000_00000000_00000000_00000000_1???????_????????_????????_????????",
  )
  private val ioAddrMask = BitPat(
    "b00000000_00000000_00000000_00000000_0001????_????????_????????_????????",
  )

  locally {
    import axi._
    read.bits.USER := 0.U
    readAddress.ready := false.B
    read.valid := false.B
    writeResponse.valid := false.B
    writeResponse.bits.ID := 0.U
    read.bits.LAST := false.B
    write.ready := false.B
    read.bits.DATA := 0.U
    read.bits.RESP := Response.Okay
    read.bits.ID := 0.U
    writeAddress.ready := false.B
    writeResponse.bits.USER := 0.U
    writeResponse.bits.RESP := Response.Okay
    simulationIO.input.ready := false.B
    simulationIO.output.bits := 0.U
    simulationIO.output.valid := false.B
  }

  val mem = SyncReadMem(sizeBytes / 8, Vec(8, UInt(8.W)))

  val sourceReady = RegInit(false.B)
  val gotSize = RegInit(false.B)
  val sourceSize = RegInit("xFFFFFFFF_FFFFFFFF".U)
  val starting = (params.instructionStart.U(64.W) >> 3).asUInt
  val sourceWriteIndex = RegInit(starting)
  when(!sourceReady) {
    when(!gotSize) {
      when(simulationSource.input.valid) {
        sourceSize := simulationSource.input.bits + starting
        sourceWriteIndex := starting
        gotSize := true.B
      }
    }.otherwise {
      when(simulationSource.input.valid) {
        mem.write(
          sourceWriteIndex,
          simulationSource.input.bits.asTypeOf(Vec(8, UInt(8.W))),
        )
        sourceWriteIndex := sourceWriteIndex + 1.U
      }
    }
    when(sourceWriteIndex === sourceSize) {
      sourceReady := true.B
    }
  }

  // WRITE OPERATION
  val writeState = Module(new FIFO(4)(new Bundle {
    val address = UInt(64.W)
    val burstLength = UInt(8.W)
  }))
  writeState.input.valid := false.B
  writeState.output.ready := false.B
  writeState.input.bits := DontCare

  val writeResponseState = Module(new FIFO(2)(new Bundle() {
    val isError = Bool()
  }))
  writeResponseState.input.valid := false.B
  writeResponseState.output.ready := false.B
  writeResponseState.input.bits := DontCare

  val burstLen = RegInit(0.U(8.W))

  when(sourceReady) {
    when(!writeState.full) {
      axi.writeAddress.ready := true.B
      when(axi.writeAddress.valid) {
        writeState.input.valid := true.B
        writeState.input.bits.address := axi.writeAddress.bits.ADDR
        writeState.input.bits.burstLength := axi.writeAddress.bits.LEN
      }
    }
    when(!writeState.empty) {
      when(writeState.output.bits.address === memAddrMask) {
        axi.write.ready := true.B
        when(axi.write.valid) {
          mem.write(
            writeState.output.bits.address(63, 3) + burstLen,
            axi.write.bits.DATA.asTypeOf(Vec(8, UInt(8.W))),
            axi.write.bits.STRB.asBools,
          )
          burstLen := burstLen + 1.U
          when(burstLen === writeState.output.bits.burstLength) {
            burstLen := 0.U
            writeState.output.ready := true.B
            writeResponseState.input.valid := true.B
            writeResponseState.input.bits.isError := false.B
          }
        }
      }.elsewhen(writeState.output.bits.address === ioAddrMask) {
        axi.write.ready := simulationIO.output.ready
        writeState.output.ready := simulationIO.output.ready
        simulationIO.output.valid := axi.write.valid
        simulationIO.output.bits := axi.write.bits.DATA
        writeResponseState.input.valid := simulationIO.output.ready
        writeResponseState.input.bits.isError := false.B
      }.otherwise {
        writeState.output.ready := true.B
        writeResponseState.input.valid := true.B
        writeResponseState.input.bits.isError := true.B
        axi.write.ready := true.B
      }
    }
    when(!writeResponseState.empty) {
      axi.writeResponse.valid := true.B
      axi.writeResponse.bits.RESP := Mux(
        writeResponseState.output.bits.isError,
        Response.DecErr,
        Response.Okay,
      )
      when(axi.writeResponse.ready) {
        writeResponseState.output.ready := true.B
      }
    }
  }

  // READ OPERATION
  val readState = Module(new FIFO(4)(new Bundle {
    val address = UInt(64.W)
    val burstLength = UInt(8.W)
  }))
  readState.input.valid := false.B
  readState.output.ready := false.B
  readState.input.bits := DontCare

  val readBurstLen = RegInit(0.U(8.W))
  val readDone = RegInit(false.B)

  val readAddr = WireDefault(0.U)
  axi.read.bits.DATA := mem.read(readAddr, !readState.empty).asUInt
  when(sourceReady) {
    when(!readState.full) {
      axi.readAddress.ready := true.B
      when(axi.readAddress.valid) {
        readState.input.valid := true.B
        readState.input.bits.address := axi.readAddress.bits.ADDR
        readState.input.bits.burstLength := axi.readAddress.bits.LEN
      }
    }
    when(!readState.empty) {
      when(readState.output.bits.address === memAddrMask) {
        readAddr := readState.output.bits
          .address(63, 3) + readBurstLen + (axi.read.ready && readDone).asUInt

        when(!readDone) {
          readDone := true.B
        }.otherwise {
          axi.read.valid := true.B
          when(axi.read.ready) {
            readBurstLen := readBurstLen + 1.U
            when(readBurstLen === readState.output.bits.burstLength) {
              axi.read.bits.LAST := true.B
              readState.output.ready := true.B
              readDone := false.B
              readBurstLen := 0.U
            }
          }
        }
      }.elsewhen(readState.output.bits.address === ioAddrMask) {
        axi.read.valid := simulationIO.input.valid
        axi.read.bits.DATA := simulationIO.input.bits
        simulationIO.input.ready := axi.read.ready
        readState.output.ready := simulationIO.input.ready && simulationIO.input.valid
      }.otherwise {
        axi.read.valid := true.B
        axi.read.bits.DATA := "xDEADBEEFDEADBEEF".U
        axi.read.bits.RESP := Response.DecErr
        readState.output.ready := axi.read.ready
      }
    }
  }
}

object SimpleAXIMemoryWithSimulationIO extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(new SimpleAXIMemoryWithSimulationIO())
}
