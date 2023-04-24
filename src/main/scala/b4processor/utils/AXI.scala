package b4processor.utils

import chisel3._
import chisel3.util._

object BurstSize extends ChiselEnum {
  val Size1 = Value(0.U(3.W))
  val Size2 = Value(1.U)
  val Size4 = Value(2.U)
  val Size8 = Value(3.U)
  val Size16 = Value(4.U)
  val Size32 = Value(5.U)
  val Size64 = Value(6.U)
  val Size128 = Value(7.U)
}

object BurstType extends ChiselEnum {
  val Fixed = Value(0.U(2.W))
  val Incr = Value(1.U)
  val Wrap = Value(2.U)
}

object Response extends ChiselEnum {
  val Okay = Value(0.U)
  val ExOkay = Value(1.U)
  val SlvErr = Value(2.U)
  val DecErr = Value(3.U)
}

object Lock extends ChiselEnum {
  val Normal = Value(0.U)
  val Exclusive = Value(1.U)
}

class AXI(
  dataWidth: Int = 32,
  addressWidth: Int = 32,
  idWidth: Int = 0,
  userReqWidth: Int = 0,
  userDataWidth: Int = 0,
  userRespWidth: Int = 0
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
    val REGION = UInt(4.W) // Optional Default 0
    val USER = UInt(userReqWidth.W)
  }

  class WriteDataChannel extends Bundle {
    val ID = UInt(idWidth.W)
    val DATA = UInt(dataWidth.W)
    val STRB = UInt((dataWidth / 8).W) // Optional Default All 1
    val LAST = Bool()
    val USER = UInt(userDataWidth.W)
  }

  class WriteResponseChannel extends Bundle {
    val ID = UInt(idWidth.W) // Optional
    val RESP = Response()
    val USER = UInt(userRespWidth.W)
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
    val REGION = UInt(4.W)
    val USER = UInt(userReqWidth.W)
  }

  class ReadDataChannel extends Bundle {
    val ID = UInt(idWidth.W)
    val DATA = UInt(dataWidth.W)
    val RESP = Response()
    val LAST = Bool()
    val USER = UInt((userDataWidth + userRespWidth).W)
  }

  val writeAddress = Irrevocable(new WriteAddressChannel)
  val write = Irrevocable(new WriteDataChannel)
  val writeResponse = Flipped(Irrevocable(new WriteResponseChannel))
  val readAddress = Irrevocable(new ReadAddressChannel)
  val read = Flipped(Irrevocable(new ReadDataChannel))
}
