package b4processor.utils.axi

import chisel3._
import chisel3.experimental.dataview._
import chisel3.util._

class ChiselAXI(
  val dataWidth: Int,
  val addressWidth: Int,
  val idWidth: Int = 0,
  val userWriteWidth: Int = 0,
  val userWriteAddressWidth: Int = 0,
  val userWriteResponceWidth: Int = 0,
  val userReadAddressWidth: Int = 0,
  val userReadWidth: Int = 0,
) extends Bundle {
  class WriteAddressChannel extends Bundle {
    val ID = UInt(idWidth.W) // Optional Default 0
    val ADDR = UInt(addressWidth.W)
    val LEN = UInt(8.W) // Optional Default 1
    val SIZE = BurstSize() // Optional Default busWidth
    val BURST = BurstType() // Optional Default Incr
    val LOCK = Lock() // Optional Default Normal
    val CACHE = UInt(4.W) // Optional Default 0
    val PROT = UInt(3.W)
    val QOS = UInt(4.W) // Optional Default 0
    val USER = UInt(userWriteAddressWidth.W)
  }

  class WriteDataChannel extends Bundle {
    val DATA = UInt(dataWidth.W)
    val STRB = UInt((dataWidth / 8).W) // Optional Default All 1
    val LAST = Bool()
    val USER = UInt(userWriteWidth.W)
  }

  class WriteResponseChannel extends Bundle {
    val ID = UInt(idWidth.W) // Optional
    val RESP = Response()
    val USER = UInt(userWriteResponceWidth.W)
  }

  class ReadAddressChannel extends Bundle {
    val ID = UInt(idWidth.W)
    val ADDR = UInt(addressWidth.W)
    val LEN = UInt(8.W)
    val SIZE = BurstSize()
    val BURST = BurstType()
    val LOCK = Lock()
    val CACHE = UInt(4.W)
    val PROT = UInt(3.W)
    val QOS = UInt(4.W)
    val USER = UInt(userReadAddressWidth.W)
  }

  class ReadDataChannel extends Bundle {
    val ID = UInt(idWidth.W)
    val DATA = UInt(dataWidth.W)
    val RESP = Response()
    val LAST = Bool()
    val USER = UInt(userReadWidth.W)
  }

  val writeAddress = Irrevocable(new WriteAddressChannel)
  val write = Irrevocable(new WriteDataChannel)
  val writeResponse = Flipped(Irrevocable(new WriteResponseChannel))
  val readAddress = Irrevocable(new ReadAddressChannel)
  val read = Flipped(Irrevocable(new ReadDataChannel))
}

object ChiselAXI {
  implicit val axiView: DataView[VerilogAXI, ChiselAXI] = DataView(
    vab =>
      new ChiselAXI(
        vab.dataWidth,
        vab.addressWidth,
        vab.idWidth,
        vab.userWriteWidth,
        vab.userWriteAddressWidth,
        vab.userWriteResponseWidth,
        vab.userReadAddressWidth,
        vab.userReadWidth,
      ),
    _.awid -> _.writeAddress.bits.ID,
    _.awaddr -> _.writeAddress.bits.ADDR,
    _.awlen -> _.writeAddress.bits.LEN,
    _.awsize -> _.writeAddress.bits.SIZE,
    _.awburst -> _.writeAddress.bits.BURST,
    _.awlock -> _.writeAddress.bits.LOCK,
    _.awcache -> _.writeAddress.bits.CACHE,
    _.awprot -> _.writeAddress.bits.PROT,
    _.awqos -> _.writeAddress.bits.QOS,
    _.awuser -> _.writeAddress.bits.USER,
    _.awvalid -> _.writeAddress.valid,
    _.awready -> _.writeAddress.ready,
    _.wdata -> _.write.bits.DATA,
    _.wstrb -> _.write.bits.STRB,
    _.wlast -> _.write.bits.LAST,
    _.wuser -> _.write.bits.USER,
    _.wvalid -> _.write.valid,
    _.wready -> _.write.ready,
    _.bid -> _.writeResponse.bits.ID,
    _.bresp -> _.writeResponse.bits.RESP,
    _.buser -> _.writeResponse.bits.USER,
    _.bvalid -> _.writeResponse.valid,
    _.bready -> _.writeResponse.ready,
    _.arid -> _.readAddress.bits.ID,
    _.araddr -> _.readAddress.bits.ADDR,
    _.arlen -> _.readAddress.bits.LEN,
    _.arsize -> _.readAddress.bits.SIZE,
    _.arburst -> _.readAddress.bits.BURST,
    _.arlock -> _.readAddress.bits.LOCK,
    _.arcache -> _.readAddress.bits.CACHE,
    _.arprot -> _.readAddress.bits.PROT,
    _.arqos -> _.readAddress.bits.QOS,
    _.aruser -> _.readAddress.bits.USER,
    _.arvalid -> _.readAddress.valid,
    _.arready -> _.readAddress.ready,
    _.rid -> _.read.bits.ID,
    _.rdata -> _.read.bits.DATA,
    _.rresp -> _.read.bits.RESP,
    _.rlast -> _.read.bits.LAST,
    _.ruser -> _.read.bits.USER,
    _.rvalid -> _.read.valid,
    _.rready -> _.read.ready,
  )
}
