package b4processor.modules.cache

import b4processor.Parameters
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class CacheFetchInterface(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val cache = new Bundle {
      val request = Decoupled(UInt(64.W))
      val response = Flipped(Decoupled(UInt(128.W)))
    }
    val fetch = new Bundle {
      val perDecoder = Vec(
        params.decoderPerThread,
        new Bundle {
          val request = Flipped(Valid(UInt(64.W)))
          val response = Valid(UInt(32.W))
        },
      )
      val requestNext = Flipped(Decoupled(UInt(64.W)))
    }
  })
  // todo remove
  locally {
    io.cache.request.valid := false.B
    io.cache.request.bits := 0.U
    io.cache.response.ready := false.B
    io.fetch.perDecoder foreach { f =>
      f.response.valid := false.B
      f.response.bits := 0.U
    }
    io.fetch.requestNext.ready := true.B
  }

  val fetchedAddressValid = RegInit(false.B)
  val fetchedAddress = Reg(UInt(64.W))
  val fetchedData = Reg(UInt(128.W))
  val prevFetchedDataTop16 = Reg(UInt(16.W))

  val fetchNew = RegInit(true.B)
  val fetchNewNow = WireDefault(fetchNew)
  when(
    io.fetch.requestNext.valid &&
      io.fetch.requestNext.bits(63, 4) =/=
      RegNext(io.fetch.requestNext.bits(63, 4)),
  ) {
    fetchNew := true.B
  }

  when(io.fetch.requestNext.valid && fetchNewNow) {
    io.cache.request.valid := true.B
    io.cache.request.bits := io.fetch.requestNext.bits(63, 4) ## 0.U(4.W)
    when(io.cache.request.ready) {
      fetchedAddress := io.fetch.requestNext.bits(63, 4) ## 0.U(4.W)
      fetchedAddressValid := false.B
      fetchNew := false.B
    }
  }

  val fetchedDataNow = WireDefault(fetchedData)
  val fetchedAddressValidNow = WireDefault(fetchedAddressValid)

  when(io.cache.response.valid) {
    fetchedAddressValid := true.B
    fetchedAddressValidNow := true.B
    fetchedData := io.cache.response.bits
    fetchedDataNow := io.cache.response.bits
    prevFetchedDataTop16 := fetchedData(127, 112) // top 16 bits
    io.cache.response.ready := true.B
  }

  // fetch response
  when(fetchedAddressValidNow) {
    io.fetch.perDecoder foreach { f =>
      f.response.bits := MuxLookup(f.request.bits(3, 1), 0.U)(
        (0 until 8 - 1).map(i => i.U -> fetchedDataNow(16 * i + 32 - 1, 16 * i)),
      )
      when(f.request.valid && f.request.bits(63, 4) === fetchedAddress(63, 4)) {
        when(f.request.bits(3, 0) === BitPat("b111?")) {
          f.response.valid := false.B
        }.otherwise {
          f.response.valid := true.B
        }
      }
    }
  }

}

object CacheFetchInterface extends App {
  implicit val params: Parameters = Parameters()
  ChiselStage.emitSystemVerilogFile(new CacheFetchInterface())
}
