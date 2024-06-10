package b4smt.modules.cache

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4smt.Parameters
import b4smt.connections.InstructionCache2Fetch
import b4smt.utils.ShiftRegister

// strategy memo
// a,f,edge
//
//cases of return
//!edge a=f
//edge a'=f a=f+1
//
//fetching
//!edge
//    a != f
//		fetch a <- f
//edge
//    !(a'=f & a=f+1)
//		a = f
//			fetch a <- f + 1
//		otherwise
//			fetch a <- f

class CacheFetchInterface(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val cache = new Bundle {
      val request = Decoupled(UInt(64.W))
      val response = Flipped(Decoupled(UInt(128.W)))
    }
    val fetch = new InstructionCache2Fetch()
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

  val fetchedAddressValid = RegInit(true.B)
  val fetchedAddressSR = ShiftRegister(UInt(64.W), 2, "xFFFFFFFF_FFFFFFFF".U)
  val fetchedData = Reg(UInt(128.W))
  val prevFetchedDataTop16 = RegInit(0.U(16.W))

  val requestingAddress = io.fetch.perDecoder(0).request.bits
  val requestingAddressValid = io.fetch.perDecoder(0).request.valid

  val isEdge = requestingAddress(3, 0) === BitPat("b111?")
  val isRequesting = RegInit(false.B)

  when(io.cache.request.valid && io.cache.request.ready) {
    isRequesting := true.B
  }.elsewhen(io.cache.response.valid && io.cache.response.ready) {
    isRequesting := false.B
  }

  when(requestingAddressValid && fetchedAddressValid) {

    when(!isEdge) {
      when(fetchedAddressSR.output(0)(63, 4) =/= requestingAddress(63, 4)) {
        io.cache.request.valid := true.B
        io.cache.request.bits :=
          requestingAddress(63, 4) ## 0.U(4.W)
        when(io.cache.request.ready) {
          fetchedAddressSR.shift(requestingAddress(63, 4) ## 0.U(4.W))
          fetchedAddressValid := false.B
          prevFetchedDataTop16 := fetchedData(127, 112) // top 16 bits
        }
      }
    }.otherwise {
      when(
        !(fetchedAddressSR.output(1)(63, 4) ===
          requestingAddress(63, 4) &&
          fetchedAddressSR.output(0)(63, 4) ===
          (requestingAddress(63, 4) + 1.U)),
      ) {
        when(fetchedAddressSR.output(0)(63, 4) === requestingAddress(63, 4)) {
          io.cache.request.valid := true.B
          io.cache.request.bits :=
            (requestingAddress(63, 4) + 1.U) ## 0.U(4.W)
          when(io.cache.request.ready) {
            fetchedAddressSR.shift((requestingAddress(63, 4) + 1.U) ## 0.U(4.W))
            fetchedAddressValid := false.B
            prevFetchedDataTop16 := fetchedData(127, 112) // top 16 bits
          }
        }.otherwise {
          io.cache.request.valid := true.B
          io.cache.request.bits :=
            requestingAddress(63, 4) ## 0.U(4.W)
          when(io.cache.request.ready) {
            fetchedAddressSR.shift(requestingAddress(63, 4) ## 0.U(4.W))
            fetchedAddressValid := false.B
            prevFetchedDataTop16 := fetchedData(127, 112) // top 16 bits
          }
        }
      }
    }
  }

  val fetchedDataNow = WireDefault(fetchedData)
  val fetchedAddressValidNow = WireDefault(fetchedAddressValid)

  io.cache.response.ready := true.B
  when(io.cache.response.valid) {
    fetchedAddressValid := true.B
    fetchedAddressValidNow := true.B
    fetchedData := io.cache.response.bits
    fetchedDataNow := io.cache.response.bits
  }

  // fetch response
  when(fetchedAddressValidNow) {
    io.fetch.perDecoder foreach { f =>
      when(f.request.valid) {
        when(f.request.bits(3, 0) === BitPat("b111?")) {
          when(
            f.request.bits(63, 4) ===
              fetchedAddressSR.output(1)(63, 4) &&
              (f.request.bits(63, 4) + 1.U) ===
              fetchedAddressSR.output(0)(63, 4),
          ) {
            f.response.valid := true.B
            f.response.bits := fetchedDataNow(15, 0) ## prevFetchedDataTop16
          }
        }.otherwise {
          when(f.request.bits(63, 4) === fetchedAddressSR.output(0)(63, 4)) {
            f.response.valid := true.B
            f.response.bits := MuxLookup(f.request.bits(3, 1), 0.U)(
              (0 until 8 - 1).map(i =>
                i.U -> fetchedDataNow(16 * i + 32 - 1, 16 * i),
              ),
            )
          }
        }
      }
    }
  }
}

object CacheFetchInterface extends App {
  implicit val params: Parameters = Parameters()
  ChiselStage.emitSystemVerilogFile(
    new CacheFetchInterface(),
    firtoolOpts = Array("--disable-all-randomization"),
  )
}
