package b4smt.modules.cache

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4smt.Parameters
import b4smt.connections.InstructionCache2Fetch

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

  val fetchedAddressValid = RegInit(false.B)
  val fetchedAddress = RegInit("xFFFFFFFF_FFFFFFFF".U)
  val fetchedAddressNow = WireDefault(fetchedAddress)
  val fetchedData = Reg(UInt(128.W))
  val prevFetchedDataTop16 = Reg(UInt(16.W))
  val nextBlock = RegInit(false.B)

  val requestingAddress = io.fetch.perDecoder(0).request.bits
  val requestingAddressValid = io.fetch.perDecoder(0).request.valid

  val fetchNew = RegInit(true.B)
  val fetchNewNow = WireDefault(fetchNew)
  val isEdge = requestingAddress(3, 0) === BitPat("b111?")
  val isRequesting = RegInit(false.B)

  when(io.cache.request.valid && io.cache.request.ready) {
    isRequesting := true.B
  }.elsewhen(io.cache.response.valid && io.cache.response.ready) {
    isRequesting := false.B
  }

  when(
    requestingAddressValid &&
      (fetchedAddress(63, 4) =/=
        requestingAddress(63, 4) || isEdge),
  ) {
    fetchNewNow := true.B
  }

  when(requestingAddressValid && fetchNewNow) {

    when(isEdge) {
      when(
        fetchedAddress(63, 4) === requestingAddress(63, 4)
          && fetchedAddressValid,
      ) {
        // 　端のアドレスかつ、現在のアドレスのデータの取得が完了している。

        io.cache.request.valid := true.B
        io.cache.request.bits :=
          (requestingAddress(63, 4) + 1.U) ## 0.U(4.W)
        when(io.cache.request.ready) {
          fetchedAddress := (requestingAddress(63, 4) + 1.U) ## 0.U(4.W)
          fetchedAddressValid := false.B
          fetchNew := false.B
          nextBlock := true.B
          prevFetchedDataTop16 := fetchedData(127, 112) // top 16 bits
        }
      }.elsewhen(fetchedAddress(63, 4) === requestingAddress(63, 4) + 1.U) {
        // 何もしない
      }.otherwise {
        // 端のアドレスででだが直前に取得したアドレスが現在以外
        io.cache.request.valid := true.B
        io.cache.request.bits := requestingAddress(63, 4) ## 0.U(4.W)
        when(io.cache.request.ready) {
          fetchedAddress := requestingAddress(63, 4) ## 0.U(4.W)
          fetchedAddressValid := false.B
          fetchNew := isEdge
          nextBlock := false.B
        }
      }
    }.otherwise {
      io.cache.request.valid := true.B
      io.cache.request.bits := requestingAddress(63, 4) ## 0.U(4.W)
      when(io.cache.request.ready) {
        fetchedAddress := requestingAddress(63, 4) ## 0.U(4.W)
        fetchedAddressValid := false.B
        fetchNew := isEdge
        nextBlock := (requestingAddress(63, 4) - fetchedAddress) === 1.U
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
      f.response.bits := MuxLookup(f.request.bits(3, 1), 0.U)(
        (0 until 8 - 1).map(i => i.U -> fetchedDataNow(16 * i + 32 - 1, 16 * i)),
      )
      when(f.request.valid) {
        when((f.request.bits(63, 4) + 1.U) === fetchedAddressNow(63, 4)) {
          when(f.request.bits(3, 0) === BitPat("b111?") && nextBlock) {
            f.response.valid := true.B
            f.response.bits := fetchedDataNow(15, 0) ## prevFetchedDataTop16
          }.otherwise {
            f.response.valid := false.B
          }
        }
        when(f.request.bits(63, 4) === fetchedAddressNow(63, 4)) {
          when(f.request.bits(3, 0) === BitPat("b111?")) {
            f.response.valid := false.B
          }.otherwise {
            f.response.valid := true.B
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