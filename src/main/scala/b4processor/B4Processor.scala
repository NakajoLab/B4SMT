package b4processor

import b4processor.connections.{DataMemoryOutput, InstructionMemory2Cache, LoadStoreQueue2Memory}
import b4processor.modules.cache.InstructionMemoryCache
import b4processor.modules.decoder.Decoder
import b4processor.modules.executor.Executor
import b4processor.modules.fetch.Fetch
import b4processor.modules.lsq.LoadStoreQueue
import b4processor.modules.memory.{DataMemory, DataMemoryBuffer}
import b4processor.modules.registerfile.RegisterFile
import b4processor.modules.reorderbuffer.ReorderBuffer
import b4processor.modules.reservationstation.ReservationStation
import chisel3._
import chisel3.experimental.FlatIO
import chisel3.stage.ChiselStage

class B4Processor(implicit params: Parameters) extends Module {
  val io = FlatIO(new Bundle {
    val instructionMemory = Flipped(new InstructionMemory2Cache)
    val dataMemory = new Bundle {
      val lsq = new LoadStoreQueue2Memory
      val output = Flipped(new DataMemoryOutput)
    }

    val registerFileContents = if (params.debug) Some(Output(Vec(31, UInt(64.W)))) else None
  })

  require(params.runParallel >= 1, "同時発行数は1以上である必要があります。")
  require(params.tagWidth >= 1, "タグ幅は1以上である必要があります。")
  require(params.fetchWidth >= 1, "フェッチ幅は1以上である必要があります。")
  require(params.maxRegisterFileCommitCount >= 1, "レジスタファイルへのコミット数は1以上である必要があります。")

  val instructionCache = Module(new InstructionMemoryCache)
  val fetch = Module(new Fetch)
  val reorderBuffer = Module(new ReorderBuffer)
  val registerFile = Module(new RegisterFile)
  val loadStoreQueue = Module(new LoadStoreQueue)
  val dataMemoryBuffer = Module(new DataMemoryBuffer)

  val decoders = (0 until params.runParallel).map(n => Module(new Decoder(n)))
  val reservationStations = Seq.fill(params.runParallel)(Module(new ReservationStation))
  val executors = Seq.fill(params.runParallel)(Module(new Executor))

  /** 命令メモリと命令キャッシュを接続 */
  io.instructionMemory <> instructionCache.io.memory

  /** レジスタのコンテンツをデバッグ時に接続 */
  if (params.debug)
    io.registerFileContents.get <> registerFile.io.values.get

  /** デコーダ同士を接続 */
  for (i <- 1 until params.runParallel)
    decoders(i - 1).io.decodersAfter <> decoders(i).io.decodersBefore

  /** 命令キャッシュとフェッチを接続 */
  instructionCache.io.fetch <> fetch.io.cache

  for (i <- 0 until params.runParallel) {
    /** フェッチとデコーダの接続 */
    decoders(i).io.instructionFetch <> fetch.io.decoders(i)

    /** デコーダとリオーダバッファを接続 */
    decoders(i).io.reorderBuffer <> reorderBuffer.io.decoders(i)

    /** デコーダとリザベーションステーションを接続 */
    decoders(i).io.reservationStation <> reservationStations(i).io.decoder

    /** リザベーションステーションと実行ユニットを接続 */
    reservationStations(i).io.executor <> executors(i).io.reservationStation

    /** リザベーションステーションとデータメモリを接続 */
    reservationStations(i).io.dataMemoryOutputValue <> io.dataMemory.output

    /** 実行ユニットとリオーダバッファを接続 */
    executors(i).io.out <> reorderBuffer.io.executors(i)

    /** デコーダとレジスタファイルの接続 */
    decoders(i).io.registerFile <> registerFile.io.decoders(i)

    /** デコーダとLSQの接続 */
    decoders(i).io.loadStoreQueue <> loadStoreQueue.io.decoders(i)

    /** デコーダと実行ユニットの接続 */
    for ((e, index) <- executors.zipWithIndex)
      decoders(i).io.executors(index) <> e.io.out

    /** デコーダとデータメモリの接続 */
    decoders(i).io.dataMemoryOutput <> io.dataMemory.output

    /** デコーダとLSQの接続 */
    loadStoreQueue.io.decoders(i) <> decoders(i).io.loadStoreQueue

    /** リザベーションステーションと実行ユニットの接続 */
    for ((e, index) <- executors.zipWithIndex)
      reservationStations(i).io.executorOutputValues(index) <> e.io.out

    /** LSQと実行ユニットの接続 */
    executors(i).io.loadStoreQueue <> loadStoreQueue.io.executors(i)

    /** フェッチと実行ユニットの接続 */
    fetch.io.executorBranchResult(i) <> executors(i).io.fetch
  }

  /** レジスタファイルとリオーダバッファ */
  registerFile.io.reorderBuffer <> reorderBuffer.io.registerFile

  /** フェッチとLSQの接続 */
  fetch.io.loadStoreQueueEmpty := loadStoreQueue.io.isEmpty

  /** フェッチとリオーダバッファの接続 */
  fetch.io.reorderBufferEmpty := reorderBuffer.io.isEmpty

  // TODO:　必要ないはずだけど、確認が必要
  //  loadStoreQueue.io.reorderBuffer <> reorderBuffer.io.loadStoreQueue

  /** データメモリバッファとLSQ */
  dataMemoryBuffer.io.dataIn <> loadStoreQueue.io.memory

  /** データメモリとデータメモリバッファ */
  io.dataMemory.lsq <> dataMemoryBuffer.io.dataOut

  /** データメモリとリオーダバッファ */
  io.dataMemory.output <> reorderBuffer.io.dataMemory

  /** フェッチと分岐予測 TODO */
  fetch.io.prediction <> DontCare
}

object B4Processor extends App {
  implicit val params = Parameters()
  (new ChiselStage).emitVerilog(new B4Processor(), args = Array("--emission-options=disableMemRandomization,disableRegisterRandomization"))
}
