package b4processor.utils

import b4processor.structures.memoryAccess.MemoryAccessInfo
import b4processor.utils.operations.{LoadStoreOperation, LoadStoreWidth}

case class LSQ2Memory(
  address: Int,
  tag: Int,
  data: Int,
  operation: LoadStoreOperation.Type,
  operationWidth: LoadStoreWidth.Type
)
