package b4processor.utils

import chisel3._

import scala.math.pow

trait FormalTools {
  def past[T <: Data](t: T, n: Int = 1) =
    Seq.fill(n)(RegNext).foldLeft(t) { case (a, b) => b(a) }

  val pastValid = RegInit(false.B)
  pastValid := true.B

  def stable[T <: Data](t: T) = t === past(t)

  def changed[T <: Data](t: T) = !(t === past(t))

  def rose(t: Bool) = !past(t) && t

  def fell(t: Bool) = past(t) && !t

  def takesEveryValue(t: UInt, msg: String = "") = {
    for (i <- 0 until pow(2, t.getWidth).toInt) {
      cover(t === i.U, s"cover value $i failed: ${msg}")
    }
  }

  def pastValidFor(t: Int) = (0 until t).map(past(pastValid, _)).reduce(_ && _)

}
