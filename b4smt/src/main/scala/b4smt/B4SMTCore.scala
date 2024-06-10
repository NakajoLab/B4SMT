package b4smt

import b4smt.connections.{
  OutputValue,
  ReservationStation2Executor,
  ReservationStation2MulDivExecutor,
  ReservationStation2PExtExecutor,
}
import b4smt.modules.AtomicLSU
import circt.stage.ChiselStage
import b4smt.modules.branch_output_collector.BranchOutputCollector
import b4smt.modules.cache.{
  CacheFetchInterface,
  DataMemoryBuffer,
  InstructionMemoryCache,
}
import b4smt.modules.csr.{CSR, CSRReservationStation}
import b4smt.modules.decoder.{Decoder, Uncompresser}
import b4smt.modules.executor.{B4PExtExecutor, Executor, MulDivExecutor}
import b4smt.modules.fetch.{Fetch, FetchBuffer}
import b4smt.modules.lsq.LoadStoreQueue
import b4smt.modules.memory.ExternalMemoryInterface
import b4smt.modules.outputcollector.{OutputCollector, OutputCollector2}
import b4smt.modules.registerfile.{RegisterFile, RegisterFileMem}
import b4smt.modules.reorderbuffer.ReorderBuffer
import b4smt.modules.reservationstation.{
  IssueBuffer,
  IssueBuffer2,
  IssueBuffer3,
  ReservationStation,
  ReservationStation2,
}
import b4smt.utils.axi.{ChiselAXI, VerilogAXI}
import chisel3._
import chisel3.experimental.dataview.DataViewable
import chisel3.util.PopCount

class B4SMTCore(implicit params: Parameters) extends Module {
  override val desiredName = "B4SMTCoreInternal"
  val axi = IO(new ChiselAXI(64, 64))

  val registerFileContents =
    if (params.debug) Some(IO(Output(Vec(params.threads, Vec(32, UInt(64.W))))))
    else None

  require(params.decoderPerThread >= 1, "スレッド毎にデコーダは1以上必要です。")
  require(params.threads >= 1, "スレッド数は1以上です。")
  require(params.tagWidth >= 1, "タグ幅は1以上である必要があります。")
  require(
    params.maxRegisterFileCommitCount >= 1,
    "レジスタファイルへのコミット数は1以上である必要があります。",
  )

  private val cacheFetchInterface =
    Seq.fill(params.threads)(Module(new CacheFetchInterface()))
  private val instructionCache =
    Seq.fill(params.threads)(Module(new InstructionMemoryCache))
  private val fetch = Seq.fill(params.threads)(Module(new Fetch))
  private val fetchBuffer = Seq.fill(params.threads)(Module(new FetchBuffer))
  private val reorderBuffer =
    Seq.fill(params.threads)(Module(new ReorderBuffer))
  private val registerFile =
    Seq.fill(params.threads)(Module(new RegisterFileMem))
  private val loadStoreQueue =
    Seq.fill(params.threads)(Module(new LoadStoreQueue))
  private val dataMemoryBuffer = Module(new DataMemoryBuffer)

  private val outputCollector = Module(new OutputCollector2)
  private val branchAddressCollector = Module(new BranchOutputCollector())

  private val uncompresser = Seq.fill(params.threads)(
    Seq.fill(params.decoderPerThread)(Module(new Uncompresser)),
  )
  private val decoders = Seq.fill(params.threads)(
    (0 until params.decoderPerThread).map(n => Module(new Decoder)),
  )
  private val reservationStation =
    Seq.fill(params.threads)(
      Seq.fill(params.decoderPerThread)(Module(new ReservationStation2)),
    )
  private val issueBuffer = Module(
    new IssueBuffer3(params.executors, new ReservationStation2Executor),
  )
  private val executors = Seq.fill(params.executors)(Module(new Executor))

  private val externalMemoryInterface = Module(new ExternalMemoryInterface)

  private val csrReservationStation =
    Seq.fill(params.threads)(Module(new CSRReservationStation))
  private val csr = Seq.fill(params.threads)(Module(new CSR))
  private val amo = Module(new AtomicLSU)

  private val issueBufferMulDiv = Module(
    new IssueBuffer3(
      params.mulDivExecutors,
      new ReservationStation2MulDivExecutor,
    ),
  )
  private val mulDivExecutor =
    Seq.fill(params.mulDivExecutors)(Module(new MulDivExecutor()))

  private val pextIssueBuffer =
    if (params.enablePExt)
      Some(
        Module(
          new IssueBuffer3(
            params.pextExecutors,
            new ReservationStation2PExtExecutor(),
          ),
        ),
      )
    else None
  private val pextExecutors =
    if (params.enablePExt)
      Some(Seq.fill(params.pextExecutors)(Module(new B4PExtExecutor())))
    else None

  axi <> externalMemoryInterface.io.coordinator

  /** レジスタのコンテンツをデバッグ時に接続 */
  if (params.debug)
    for (tid <- 0 until params.threads)
      for (i <- 0 until 32)
        registerFileContents.get(tid)(i) <> registerFile(tid).io.values.get(i)
  if (params.enablePExt)
    for (pe <- 0 until params.pextExecutors) {
      pextIssueBuffer.get.io.executors(pe) <> pextExecutors.get(pe).io.input
      outputCollector.io.pextExecutor(pe) <> pextExecutors.get(pe).io.output
    }
  else
    outputCollector.io.pextExecutor foreach { o =>
      o.valid := false.B
      o.bits := 0.U.asTypeOf(new OutputValue())
    }

  for (me <- 0 until params.mulDivExecutors) {
    issueBufferMulDiv.io.executors(me) <>
      mulDivExecutor(me).io.reservationStation
    outputCollector.io.mulDivExecutor(me) <>
      mulDivExecutor(me).io.out
  }

  for (e <- 0 until params.executors) {

    /** リザベーションステーションと実行ユニットを接続 */
    issueBuffer.io.executors(e) <> executors(e).io.reservationStation

    /** 出力コレクタと実行ユニットの接続 */
    outputCollector.io.executor(e) <> executors(e).io.out

    /** 分岐結果コレクタと実行ユニットの接続 */
    branchAddressCollector.io.executor(e) <> executors(e).io.fetch
  }

  for (tid <- 0 until params.threads) {
    amo.io.collectedOutput := outputCollector.io.outputs
    amo.io.output <> outputCollector.io.amo
    amo.io.reorderBuffer(tid) <> reorderBuffer(tid).io.loadStoreQueue

    csrReservationStation(tid).io.reorderBuffer <>
      reorderBuffer(tid).io.loadStoreQueue
    csrReservationStation(tid).io.isError := reorderBuffer(tid).io.isError

    csr(tid).io.threadId := tid.U
    instructionCache(tid).io.threadId := tid.U
    reorderBuffer(tid).io.threadId := tid.U
    registerFile(tid).io.threadId := tid.U

    /** 命令キャッシュとフェッチを接続 */
    instructionCache(tid).io.fetch <> cacheFetchInterface(tid).io.cache
    cacheFetchInterface(tid).io.fetch <> fetch(tid).io.cache

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

    branchAddressCollector.io.isError(tid) := reorderBuffer(tid).io.isError
    fetch(tid).io.isError := reorderBuffer(tid).io.isError

    /** フェッチとデコーダの接続 */
    for (d <- 0 until params.decoderPerThread) {
      reservationStation(tid)(d).io.collectedOutput :=
        outputCollector.io.outputs(tid)

      reservationStation(tid)(d).io.threadId := tid.U

      fetch(tid).io.interrupt := false.B
      reservationStation(tid)(d).io.issue <>
        issueBuffer.io.reservationStations(tid)(d)
      if (params.enablePExt)
        reservationStation(tid)(d).io.pextIssue <>
          pextIssueBuffer.get.io.reservationStations(tid)(d)
      else
        reservationStation(tid)(d).io.pextIssue.ready := false.B

      reservationStation(tid)(d).io.mulDivIssue <>
        issueBufferMulDiv.io.reservationStations(tid)(d)

      amo.io.decoders(tid)(d) <> decoders(tid)(d).io.amo

      decoders(tid)(d).io.csr <> csrReservationStation(tid).io.decoderInput(d)

      decoders(tid)(d).io.threadId := tid.U

      uncompresser(tid)(d).io.fetch <> fetchBuffer(tid).io.output(d)

      /** デコーダとフェッチバッファ */
      decoders(tid)(d).io.instructionFetch <> uncompresser(tid)(d).io.decoder

      /** デコーダとリオーダバッファを接続 */
      decoders(tid)(d).io.reorderBuffer <> reorderBuffer(tid).io.decoders(d)

      /** デコーダとリザベーションステーションを接続 */
      reservationStation(tid)(d).io.decoder <>
        decoders(tid)(d).io.reservationStation

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
    fetch(tid).io.loadStoreQueueEmpty := loadStoreQueue(tid).io.empty

    /** フェッチとリオーダバッファの接続 */
    fetch(tid).io.reorderBufferEmpty := reorderBuffer(tid).io.isEmpty

    /** データメモリバッファとLSQ */
    dataMemoryBuffer.io.dataIn(tid) <> loadStoreQueue(tid).io.memory

    /** リオーダバッファと出力コレクタ */
    reorderBuffer(tid).io.collectedOutputs := outputCollector.io.outputs(tid)

    /** リオーダバッファとLSQ */
    reorderBuffer(tid).io.loadStoreQueue <> loadStoreQueue(tid).io.reorderBuffer

    /** 命令メモリと命令キャッシュを接続 */
    externalMemoryInterface.io.instruction(tid) <>
      instructionCache(tid).io.memory

    csr(tid).io.activeLr := amo.io.statusLr(tid)
    csr(tid).io.activeSc := amo.io.statusSc(tid)
    csr(tid).io.activeScFail := amo.io.statusScFail(tid)
    csr(tid).io.activeAmo := amo.io.statusAmo(tid)
    csr(tid).io.activeLoad := loadStoreQueue(tid).io.statusLoad
    csr(tid).io.activeStore := loadStoreQueue(tid).io.statusStore
    csr(tid).io.activeError := reorderBuffer(tid).io.isError
    csr(tid).io.activeExecutor := PopCount(executors.map(_.io.status(tid)))
    if (pextExecutors.isDefined) {
      csr(tid).io.activePext := PopCount(
        pextExecutors.get.map(_.io.status(tid)),
      )
    } else {
      csr(tid).io.activePext := 0.U
    }
    csr(tid).io.fullReorderBuffer := reorderBuffer(tid).io.full
    csr(tid).io.fullLoadStore := loadStoreQueue(tid).io.full
    csr(tid).io.fullAmo := amo.io.statusFull(tid)
    csr(tid).io.fullReservationStation := PopCount(
      reservationStation(tid).map(_.io.full),
    )

    instructionCache(tid).io.flush <> fetch(tid).io.icache_flush

    /** フェッチと分岐予測 TODO */
    fetch(tid).io.prediction <> DontCare
  }

  /** メモリとデータメモリバッファ */
  externalMemoryInterface.io.data <> dataMemoryBuffer.io.memory
  dataMemoryBuffer.io.output <> outputCollector.io.dataMemory
  externalMemoryInterface.io.amo <> amo.io.memory
}

class B4SMTCoreFixedPorts(implicit params: Parameters) extends RawModule {
  override val desiredName = "B4SMTCore"
  val AXI_MM = IO(new VerilogAXI(64, 64))
  val axi = AXI_MM.viewAs[ChiselAXI]

  val aclk = IO(Input(Bool()))
  val aresetn = IO(Input(Bool()))

  withClockAndReset(aclk.asClock, (!aresetn).asAsyncReset) {
    val core = Module(new B4SMTCore())
    core.axi <> axi
  }
}

object B4SMTCore extends App {
  implicit val params: b4smt.Parameters = Parameters(
    threads = 4,
    executors = 2,
    decoderPerThread = 1,
    maxRegisterFileCommitCount = 1,
    tagWidth = 4,
    parallelOutput = 1,
    instructionStart = 0x2000_0000L,
    enablePExt = true,
    pextExecutors = 1,
  )

  ChiselStage.emitSystemVerilogFile(
    new B4SMTCoreFixedPorts(),
    Array.empty,
    Array(
//      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb",
      "-O=release",
      "--emit-omir",
      "--export-module-hierarchy",
      "--disable-all-randomization",
      "--add-vivado-ram-address-conflict-synthesis-bug-workaround",
    ),
  )
}
