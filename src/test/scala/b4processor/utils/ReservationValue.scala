package b4processor.utils

import b4processor.utils.operations.ALUOperation

case class ReservationValue(
  valid: Boolean = true,
  destinationTag: Int = 0,
  value1: BigInt = 0,
  value2: BigInt = 0,
  operation: ALUOperation.Type = ALUOperation.BranchEqual,
  wasCompressed: Boolean = false,
  branchOffset: Int = 0,
)
