package b4processor.utils

import b4processor.Parameters
import chisel3._

trait ForPext {
  def forPext[T <: Data](t: T)(implicit params: Parameters) =
    if (params.enablePExt) Some(t) else None
}
