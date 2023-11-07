package b4processor.utils

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class PriorityArbiterWithIndex[T <: Data](gen: T, n: Int, priorities: Seq[Int])
    extends Module {

  val in = IO(Vec(n, Flipped(Decoupled(gen))))
  val out = IO(Decoupled(new IndexedData(gen, n)))

  require(n == priorities.length)
  require(n > 0)

  private val indexedIn = in.zipWithIndex.map {
    case (input, idx) => {
      val w = Wire(Decoupled(new IndexedData(gen, n)))
      w.valid <> input.valid
      w.ready <> input.ready
      w.bits.index := idx.U
      w.bits.data := input.bits
      w
    }
  }

  if (n == 1) {
    out <> indexedIn(0)
  } else {
    val priority_set_sorted = priorities.toSet.toSeq.sorted
    if (priority_set_sorted.size == 1) {
      val rr_arb = Module(new B4RRArbiter(new IndexedData(gen, n), n))
      rr_arb.io.in zip indexedIn foreach { case (a, i) => a <> i }
      out <> rr_arb.io.out
    } else {
      val final_arb = Module(
        new Arbiter(new IndexedData(gen, n), priority_set_sorted.size),
      )
      for ((p, a) <- priority_set_sorted zip final_arb.io.in) {
        if (priorities.count(v => v == p) == 1) {
          val in_index =
            priorities.zipWithIndex.filter(v => v._1 == p).map(v => v._2).head
          a <> indexedIn(in_index)
        } else {
          val rr_arb = Module(
            new B4RRArbiter(
              new IndexedData(gen, n),
              priorities.count(v => v == p),
            ),
          )
          val in_indexes =
            priorities.zipWithIndex.filter(v => v._1 == p).map(v => v._2)
          for ((inIdx, ra) <- in_indexes zip rr_arb.io.in) {
            ra <> indexedIn(inIdx)
          }
          a <> rr_arb.io.out
        }
      }
      out <> final_arb.io.out

    }
  }
}

class IndexedData[T <: Data](gen: T, n_max: Int) extends Bundle {
  val data = gen
  val index = UInt(log2Up(n_max).W)
}

object PriorityArbiterWithIndex extends App {
  ChiselStage.emitSystemVerilogFile(
    new PriorityArbiterWithIndex(UInt(4.W), 5, Seq(1, 1, 2, 2, 3)),
  )
}
