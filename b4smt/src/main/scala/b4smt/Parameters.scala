package b4smt

import scala.math.pow

/** Parameters for generating the processor
  *
  * @param tagWidth
  *   Number of bits in tags used by the reorder buffer
  * @param loadStoreQueueIndexWidth
  *   Bit width of indices used by the load-store queue
  * @param maxRegisterFileCommitCount
  *   Number of instructions committed from the reorder buffer to the register
  *   file per clock (Max)
  * @param maxDataMemoryCommitCount
  *   Maximum number of outputs to the memory buffer
  * @param debug
  *   Enable debugging features
  * @param fetchWidth
  *   Number of instructions retrieved from memory during instruction fetch
  * @param branchPredictionWidth
  *   Number of low-order bits used for branch prediction
  * @param instructionStart
  *   Initial program counter value
  */
case class Parameters(
  tagWidth: Int = 4,
  executors: Int = 2,
  mulDivExecutors: Int = 1,
  loadStoreQueueIndexWidth: Int = 3,
  loadStoreQueueCheckLength: Int = 3,
  decoderPerThread: Int = 2,
  threads: Int = 2,
  maxRegisterFileCommitCount: Int = 1,
  maxDataMemoryCommitCount: Int = 1,
  fetchWidth: Int = 2,
  branchPredictionWidth: Int = 4,
  parallelOutput: Int = 1,
  instructionStart: Long = 0x8010_0000L,
  debug: Boolean = false,
  enablePExt: Boolean = false,
  pextExecutors: Int = 1,
  reservationStationWidth: Int = 4,
  // Instruction cache parameters
  ICacheWay: Int = 2,
  ICacheSet: Int = 128,
  ICacheBlockWidth: Int = 512,
  ICacheDataNum: Int = 4,
  MemoryBurstLength: Int = 8,
)
