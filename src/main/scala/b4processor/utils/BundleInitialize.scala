package b4processor.utils

import chisel3._
import chisel3.experimental.SourceInfo

object BundleInitialize {
  implicit class AddBundleInitializeConstructor[T <: Record](x: T) {
    def initialize(
      elems: (T => (Data, Data))*
    )(implicit sourceInfo: SourceInfo): T = {
      val w = Wire(x)
      for (e <- elems) {
        e(w)._1 := e(w)._2
      }
      w
    }
  }
}
