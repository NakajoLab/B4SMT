package b4smt.utils

import b4smt.structures.memoryAccess.MemoryAccessInfo
import b4smt.utils.operations.{LoadStoreOperation, LoadStoreWidth}

case class LSQ2Memory(
  address: Int,
  tag: Int,
  data: Int,
  operation: LoadStoreOperation.Type,
  operationWidth: LoadStoreWidth.Type,
)
