package b4processor.utils.axi

import chisel3._
object BurstSize extends ChiselEnum {

  /** 8 bits */
  val Size1 = Value(0.U(3.W))

  /** 16 bits */
  val Size2 = Value(1.U)

  /** 32 bits */
  val Size4 = Value(2.U)

  /** 64 bits */
  val Size8 = Value(3.U)

  /** 128 bits */
  val Size16 = Value(4.U)

  /** 256 bits */
  val Size32 = Value(5.U)

  /** 512 bits */
  val Size64 = Value(6.U)

  /** 1024 bits */
  val Size128 = Value(7.U)
}

object BurstType extends ChiselEnum {
  val Fixed = Value(0.U(2.W))
  val Incr = Value(1.U)
  val Wrap = Value(2.U)
  val _Reserved = Value(3.U)
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
