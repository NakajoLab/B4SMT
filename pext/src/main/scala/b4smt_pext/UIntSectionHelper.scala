package b4smt_pext

import chisel3._
import chisel3.util._

import scala.math.pow

object UIntSectionHelper {
  implicit class UIntSection(u: UInt) {
    def W(x: Int) = u(x * 32 + 31, x * 32)
    def H(x: Int) = u(x * 16 + 15, x * 16)
    def B(x: Int) = u(x * 8 + 7, x * 8)
  }

  def SE(n: Int)(x: UInt) = {
    require(x.getWidth <= n)
    val t = Wire(SInt(n.W))
    t := x.asSInt
    t.asUInt
  }

  def SE17(x: UInt) = SE(17)(x)
  def SE64(x: UInt) = SE(64)(x)
  def SE65(x: UInt) = SE(65)(x)
  def SE33(x: UInt) = SE(33)(x)

  def SE16(x: UInt) = SE(16)(x)

  def SE9(x: UInt) = SE(9)(x)

  def ZE(n: Int)(x: UInt) = {
    require(x.getWidth <= n)
    val t = Wire(UInt(n.W))
    t := x
    t
  }

  def ZE17(x: UInt) = ZE(17)(x)
  def ZE33(x: UInt) = ZE(33)(x)
  def ZE16(x: UInt) = ZE(16)(x)

  def ZE9(x: UInt) = ZE(9)(x)

  object SAT {

    def Q(n: Int) = (x: UInt) => {
      val v = x.asSInt
      val max = (pow(2, n) - 1).toInt.S
      val min = -pow(2, n).toInt.S
      (
        Mux(v > max, max, Mux(v < min, min, v)).asUInt(n, 0),
        Mux(v > max, true.B, Mux(v < min, true.B, false.B)),
      )
    }

    def Q31 = Q(31)
    def Q63 = Q(63)

    def Q15 = Q(15)

    def Q7 = Q(7)

    def Q(n: UInt) = (x: UInt) => {
      val v = x.asSInt
      val amt = n

      (
        MuxLookup(amt, 0.U)(
          (0 until pow(2, n.getWidth).toInt).map(i =>
            i.U -> {
              val max = (pow(2, i) - 1).toInt.S
              val min = -pow(2, i).toInt.S
              Mux(v > max, max, Mux(v < min, min, v)).asUInt
            },
          ),
        ),
        MuxLookup(amt, false.B)(
          (0 until pow(2, n.getWidth).toInt).map(i =>
            i.U -> {
              val max = (pow(2, i) - 1).toInt.S
              val min = -pow(2, i).toInt.S
              Mux(v > max, true.B, Mux(v < min, true.B, false.B))
            },
          ),
        ),
      )
    }

    def U(n: UInt) = (x: UInt) => {
      val amt = n
      (
        MuxLookup(amt, 0.U)(
          (0 until pow(2, n.getWidth).toInt).map(i =>
            i.U -> {
              val max = (pow(2, i) - 1).toInt.U
              Mux(x > max, max, x)
            },
          ),
        ),
        MuxLookup(amt, false.B)(
          (0 until pow(2, n.getWidth).toInt).map(i =>
            i.U -> {
              val max = (pow(2, i) - 1).toInt.U
              Mux(x > max, true.B, false.B)
            },
          ),
        ),
      )
    }

    def U(n: Int) = (x: UInt) => {
      val max = (pow(2, n) - 1).toInt.U
      (Mux(x > max, max, x), Mux(x > max, true.B, false.B))
    }
    def U64 = U(64)

    def U32 = U(32)

    def U16 = U(16)

    def U8 = U(8)

  }

  def ABS(x: UInt) = {
    val v = x.asSInt
    v.abs.asUInt
  }

  def RoundingShiftRightUnsigned16(x: UInt, shamt: UInt) = {
    val amt = shamt(3, 0)
    Mux(amt === 0.U, x, ZE17((x >> (amt - 1.U)).asUInt + 1.U)(16, 1))
  }
  def RoundingShiftRightSigned16(x: UInt, shamt: UInt) = {
    val amt = shamt(3, 0)
    Mux(
      amt === 0.U,
      x,
      SE17(((x.asSInt >> (amt - 1.U)).asSInt + 1.S).asUInt)(16, 1),
    )
  }

  def RoundingShiftRightUnsigned32(x: UInt, shamt: UInt) = {
    val amt = shamt(4, 0)
    Mux(amt === 0.U, x, ZE33((x >> (amt - 1.U)).asUInt + 1.U)(32, 1))
  }

  def RoundingShiftRightSigned32(x: UInt, shamt: UInt) = {
    val amt = shamt(4, 0)
    Mux(
      amt === 0.U,
      x,
      SE33(((x.asSInt >> (amt - 1.U)).asSInt + 1.S).asUInt)(32, 1),
    )
  }

  def SaturatingShiftLeft16(x: UInt, shamt: UInt) = {
    val amt = shamt(3, 0)
    val shifted = (x << amt).asUInt
    SAT.Q15(shifted)
  }

  def SaturatingShiftLeft32(x: UInt, shamt: UInt) = {
    val amt = shamt(4, 0)
    val shifted = (x << amt).asUInt
    SAT.Q31(shifted)
  }

  def RoundingShiftRightUnsigned8(x: UInt, shamt: UInt) = {
    val amt = shamt(2, 0)
    Mux(amt === 0.U, x, ZE9((x >> (amt - 1.U)).asUInt + 1.U)(8, 1))
  }

  def RoundingShiftRightSigned8(x: UInt, shamt: UInt) = {
    val amt = shamt(2, 0)
    Mux(
      amt === 0.U,
      x,
      SE9(((x.asSInt >> (amt - 1.U)).asSInt + 1.S).asUInt)(8, 1),
    )
  }

  def SaturatingShiftLeft8(x: UInt, shamt: UInt) = {
    val amt = shamt(2, 0)
    val shifted = (x << amt).asUInt
    SAT.Q7(shifted)
  }

  def ROUND(x: UInt, lsb: UInt) = x + lsb

  def CLRS(x: UInt) = {
    MuxCase(
      0.U,
      (0 until x.getWidth - 1).reverse.map(i => {
        (x(x.getWidth - 1) === x(i)) -> (x.getWidth - i).U
      }),
    )
  }

  def CLZ(x: UInt) = {
    MuxCase(
      0.U,
      (0 until x.getWidth).reverse.map(i => {
        (x(i) === 0.U) -> (x.getWidth - i).U
      }),
    )
  }
}
