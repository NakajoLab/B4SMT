package b4smt.utils

import b4smt.utils.operations.{LoadStoreOperation, LoadStoreWidth}

case class DecodeEnqueue(
  operation: LoadStoreOperation.Type,
  operationWidth: LoadStoreWidth.Type,
  addressTag: Int,
  storeDataTag: Int,
  storeData: Option[Long],
)
