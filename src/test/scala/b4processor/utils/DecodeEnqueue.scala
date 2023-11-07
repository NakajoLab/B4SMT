package b4processor.utils

import b4processor.utils.operations.{LoadStoreOperation, LoadStoreWidth}

case class DecodeEnqueue(
  operation: LoadStoreOperation.Type,
  operationWidth: LoadStoreWidth.Type,
  addressTag: Int,
  storeDataTag: Int,
  storeData: Option[Long],
)
