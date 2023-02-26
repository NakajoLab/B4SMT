package b4processor

import b4processor.modules.branch_output_collector.BranchOutputCollector
import b4processor.modules.cache.{DataMemoryBuffer, InstructionMemoryCache}
import b4processor.modules.csr.{CSR, CSRReservationStation}
import b4processor.modules.decoder.{Decoder, Uncompresser}
import b4processor.modules.executor.Executor
import b4processor.modules.fetch.{Fetch, FetchBuffer}
import b4processor.modules.lsq.LoadStoreQueue
import b4processor.modules.memory.ExternalMemoryInterface
import b4processor.modules.outputcollector.OutputCollector
import b4processor.modules.registerfile.RegisterFile
import b4processor.modules.reorderbuffer.ReorderBuffer
import b4processor.modules.reservationstation.ReservationStation
import b4processor.utils.AXI
import chisel3._
import chisel3.stage.ChiselStage

class B4Processor(implicit params: Parameters) extends Module {
  val axi = IO(new AXI(64, 64))
  val registerFileContents =
    if (params.debug) Some(IO(Output(Vec(params.threads, Vec(32, UInt(64.W))))))
    else None

  require(params.decoderPerThread >= 1, "スレッド毎にデコーダは1以上必要です。")
  require(params.threads >= 1, "スレッド数は1以上です。")
  require(params.tagWidth >= 1, "タグ幅は1以上である必要があります。")
  require(
    params.maxRegisterFileCommitCount >= 1,
    "レジスタファイルへのコミット数は1以上である必要があります。"
  )

  private val instructionCache =
    Seq.fill(params.threads)(Module(new InstructionMemoryCache))
  private val fetch = Seq.fill(params.threads)(Module(new Fetch))
  private val fetchBuffer = Seq.fill(params.threads)(Module(new FetchBuffer))
  private val reorderBuffer =
    Seq.fill(params.threads)(Module(new ReorderBuffer))
  private val registerFile = Seq.fill(params.threads)(Module(new RegisterFile))
  private val loadStoreQueue =
    Seq.fill(params.threads)(Module(new LoadStoreQueue))
  private val dataMemoryBuffer = Module(new DataMemoryBuffer)

  private val outputCollector = Module(new OutputCollector)
  private val branchAddressCollector = Module(new BranchOutputCollector())

  private val uncompresser = Seq.fill(params.threads)(
    Seq.fill(params.decoderPerThread)(Module(new Uncompresser))
  )
  private val decoders = Seq.fill(params.threads)(
    (0 until params.decoderPerThread).map(n => Module(new Decoder))
  )
  private val reservationStation = Module(new ReservationStation)
  private val executors = Seq.fill(params.executors)(Module(new Executor))

  private val externalMemoryInterface = Module(new ExternalMemoryInterface)

  private val csrReservationStation =
    Seq.fill(params.threads)(Module(new CSRReservationStation))
  private val csr = Seq.fill(params.threads)(Module(new CSR))

  axi <> externalMemoryInterface.io.coordinator

  /** 出力コレクタとデータメモリ */
  outputCollector.io.dataMemory <> externalMemoryInterface.io.dataReadOut

  /** レジスタのコンテンツをデバッグ時に接続 */
  if (params.debug) {
    for (tid <- 0 until params.threads)
      for (i <- 0 until 32)
        registerFileContents.get(tid)(i) <> registerFile(tid).io.values.get(i)
  }

  for (e <- 0 until params.executors) {

    /** リザベーションステーションと実行ユニットを接続 */
    reservationStation.io.executor(e) <> executors(e).io.reservationStation

    /** 出力コレクタと実行ユニットの接続 */
    outputCollector.io.executor(e) <> executors(e).io.out

    /** 分岐結果コレクタと実行ユニットの接続 */
    branchAddressCollector.io.executor(e) <> executors(e).io.fetch
  }

  /** リザベーションステーションと実行ユニットの接続 */
  reservationStation.io.collectedOutput := outputCollector.io.outputs

  for (tid <- 0 until params.threads) {
    csr(tid).io.threadId := tid.U
    instructionCache(tid).io.threadId := tid.U
    reorderBuffer(tid).io.threadId := tid.U
    registerFile(tid).io.threadId := tid.U

    /** 命令キャッシュとフェッチを接続 */
    instructionCache(tid).io.fetch <> fetch(tid).io.cache

    /** フェッチとフェッチバッファの接続 */
    fetch(tid).io.fetchBuffer <> fetchBuffer(tid).io.input

    fetch(tid).io.threadId := tid.U

    csrReservationStation(tid).io.toCSR <> csr(tid).io.decoderInput

    csr(tid).io.CSROutput <> outputCollector.io.csr(tid)

    csrReservationStation(tid).io.output <> outputCollector.io.outputs(tid)

    reorderBuffer(tid).io.csr <> csr(tid).io.reorderBuffer

    fetch(tid).io.csr <> csr(tid).io.fetch

    fetch(tid).io.csrReservationStationEmpty :=
      csrReservationStation(tid).io.empty

    outputCollector.io.isError(tid) := reorderBuffer(tid).io.isError
    branchAddressCollector.io.isError(tid) := reorderBuffer(tid).io.isError
    fetch(tid).io.isError := reorderBuffer(tid).io.isError

    /** フェッチとデコーダの接続 */
    for (d <- 0 until params.decoderPerThread) {
      decoders(tid)(d).io.csr <> csrReservationStation(tid).io.decoderInput(d)

      decoders(tid)(d).io.threadId := tid.U

      uncompresser(tid)(d).io.fetch <> fetchBuffer(tid).io.output(d)

      /** デコーダとフェッチバッファ */
      decoders(tid)(d).io.instructionFetch <> uncompresser(tid)(d).io.decoder

      /** デコーダとリオーダバッファを接続 */
      decoders(tid)(d).io.reorderBuffer <> reorderBuffer(tid).io.decoders(d)

      /** デコーダとリザベーションステーションを接続 */
      decoders(tid)(d).io.reservationStation <>
        reservationStation.io.decoder(tid + d * params.threads)

      /** デコーダとレジスタファイルの接続 */
      decoders(tid)(d).io.registerFile <> registerFile(tid).io.decoders(d)

      /** デコーダとLSQの接続 */
      decoders(tid)(d).io.loadStoreQueue <> loadStoreQueue(tid).io.decoders(d)

      /** デコーダと出力コレクタ */
      decoders(tid)(d).io.outputCollector := outputCollector.io.outputs(tid)

      /** デコーダとLSQの接続 */
      loadStoreQueue(tid).io.decoders(d) <> decoders(tid)(d).io.loadStoreQueue
    }

    /** フェッチと分岐結果の接続 */
    fetch(tid).io.collectedBranchAddresses :=
      branchAddressCollector.io.fetch(tid)

    /** LSQと出力コレクタ */
    loadStoreQueue(tid).io.outputCollector := outputCollector.io.outputs(tid)

    /** レジスタファイルとリオーダバッファ */
    registerFile(tid).io.reorderBuffer <> reorderBuffer(tid).io.registerFile

    /** フェッチとLSQの接続 */
    fetch(tid).io.loadStoreQueueEmpty := loadStoreQueue(tid).io.isEmpty

    /** フェッチとリオーダバッファの接続 */
    fetch(tid).io.reorderBufferEmpty := reorderBuffer(tid).io.isEmpty

    /** データメモリバッファとLSQ */
    dataMemoryBuffer.io.dataIn(tid) <> loadStoreQueue(tid).io.memory

    /** リオーダバッファと出力コレクタ */
    reorderBuffer(tid).io.collectedOutputs := outputCollector.io.outputs(tid)

    /** リオーダバッファとLSQ */
    reorderBuffer(tid).io.loadStoreQueue <> loadStoreQueue(tid).io.reorderBuffer

    /** 命令メモリと命令キャッシュを接続 */
    externalMemoryInterface.io.instructionFetchRequest(tid) <>
      instructionCache(tid).io.memory.request
    externalMemoryInterface.io.instructionOut(tid) <>
      instructionCache(tid).io.memory.response

    /** フェッチと分岐予測 TODO */
    fetch(tid).io.prediction <> DontCare
  }

  /** メモリとデータメモリバッファ */
  externalMemoryInterface.io.dataReadRequests <> dataMemoryBuffer.io.dataReadRequest
  externalMemoryInterface.io.dataWriteRequests <> dataMemoryBuffer.io.dataWriteRequest
}

object B4Processor extends App {
  implicit val params = Parameters(
    threads = 2,
    executors = 2,
    decoderPerThread = 1,
    maxRegisterFileCommitCount = 1,
    tagWidth = 4,
    instructionStart = 0x2000_0000L
  )
//  ChiselStage.emitSystemVerilogFile(new B4Processor())
  (new ChiselStage).emitSystemVerilog(new B4Processor)
}
