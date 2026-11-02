package b4smt

import b4smt.connections.{
  OutputValue,
  LoadStoreQueue2Memory,
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
import chisel3.connectable._
import chisel3.experimental.dataview.DataViewable
import chisel3.util.{PopCount, ReadyValidIO}

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

  axi :<>= externalMemoryInterface.io.coordinator

  /** Connect register contents when debugging */
  if (params.debug)
    for (tid <- 0 until params.threads)
      for (i <- 0 until 32)
        registerFileContents.get(tid)(i) := registerFile(tid).io.values.get(i)
  if (params.enablePExt)
    for (pe <- 0 until params.pextExecutors) {
      pextExecutors.get(pe).io.input :<>= pextIssueBuffer.get.io.executors(pe)

      (outputCollector.io.pextExecutor(pe): ReadyValidIO[OutputValue]) :<>=
        (pextExecutors.get(pe).io.output: ReadyValidIO[OutputValue])
    }
  else
    outputCollector.io.pextExecutor foreach { o =>
      o.valid := false.B
      o.bits := 0.U.asTypeOf(new OutputValue())
    }

  for (me <- 0 until params.mulDivExecutors) {
    mulDivExecutor(me).io.reservationStation :<>=
      issueBufferMulDiv.io.executors(me)

    (outputCollector.io.mulDivExecutor(me): ReadyValidIO[OutputValue]) :<>=
      (mulDivExecutor(me).io.out: ReadyValidIO[OutputValue])
  }

  for (e <- 0 until params.executors) {

    /** Connect the reservation station and execution units */
    (executors(e).io.reservationStation: ReadyValidIO[
      ReservationStation2Executor,
    ]) :<>=
      (issueBuffer.io.executors(e): ReadyValidIO[ReservationStation2Executor])

    /** Connect the output collector and execution units */
    outputCollector.io.executor(e) :<>= executors(e).io.out

    /** Connect the branch result collector and execution units */
    branchAddressCollector.io.executor(e) :<>= executors(e).io.fetch
  }

  for (tid <- 0 until params.threads) {
    amo.io.collectedOutput := outputCollector.io.outputs

    (outputCollector.io.amo: ReadyValidIO[OutputValue]) :<>=
      (amo.io.output: ReadyValidIO[OutputValue])
    amo.io.reorderBuffer(tid) := reorderBuffer(tid).io.loadStoreQueue

    csrReservationStation(tid).io.reorderBuffer :=
      reorderBuffer(tid).io.loadStoreQueue
    csrReservationStation(tid).io.isError := reorderBuffer(tid).io.isError

    csr(tid).io.threadId := tid.U
    instructionCache(tid).io.threadId := tid.U
    reorderBuffer(tid).io.threadId := tid.U
    registerFile(tid).io.threadId := tid.U

    /** Connect the instruction cache and fetch */
    instructionCache(tid).io.fetch.request :<>= cacheFetchInterface(
      tid,
    ).io.cache.request
    cacheFetchInterface(tid).io.cache.response :<>= instructionCache(
      tid,
    ).io.fetch.response
    fetch(tid).io.cache :<>= cacheFetchInterface(tid).io.fetch

    /** Connect fetch and the fetch buffer */
    fetchBuffer(tid).io.input :<>= fetch(tid).io.fetchBuffer

    fetch(tid).io.threadId := tid.U

    csr(tid).io.decoderInput :<>= csrReservationStation(tid).io.toCSR

    outputCollector.io.csr(tid) :<>= csr(tid).io.CSROutput

    csrReservationStation(tid).io.output := outputCollector.io.outputs(tid)

    csr(tid).io.reorderBuffer := reorderBuffer(tid).io.csr

    fetch(tid).io.csr := csr(tid).io.fetch

    fetch(tid).io.csrReservationStationEmpty :=
      csrReservationStation(tid).io.empty

    branchAddressCollector.io.isError(tid) := reorderBuffer(tid).io.isError
    fetch(tid).io.isError := reorderBuffer(tid).io.isError

    /** Connect fetch and the decoder */
    for (d <- 0 until params.decoderPerThread) {
      reservationStation(tid)(d).io.collectedOutput :=
        outputCollector.io.outputs(tid)

      reservationStation(tid)(d).io.threadId := tid.U

      fetch(tid).io.interrupt := false.B

      (issueBuffer.io.reservationStations(tid)(d): ReadyValidIO[
        ReservationStation2Executor,
      ]) :<>=
        (reservationStation(tid)(d).io.issue: ReadyValidIO[
          ReservationStation2Executor,
        ])
      if (params.enablePExt)
        (pextIssueBuffer.get.io.reservationStations(tid)(d): ReadyValidIO[
          ReservationStation2PExtExecutor,
        ]) :<>=
          (reservationStation(tid)(d).io.pextIssue: ReadyValidIO[
            ReservationStation2PExtExecutor,
          ])
      else
        reservationStation(tid)(d).io.pextIssue.ready := false.B

      (issueBufferMulDiv.io.reservationStations(tid)(d): ReadyValidIO[
        ReservationStation2MulDivExecutor,
      ]) :<>=
        (reservationStation(tid)(d).io.mulDivIssue: ReadyValidIO[
          ReservationStation2MulDivExecutor,
        ])

      amo.io.decoders(tid)(d) :<>= decoders(tid)(d).io.amo

      csrReservationStation(tid).io.decoderInput(d) :<>= decoders(tid)(d).io.csr

      decoders(tid)(d).io.threadId := tid.U

      uncompresser(tid)(d).io.fetch :<>= fetchBuffer(tid).io.output(d)

      /** Decoder and fetch buffer */
      decoders(tid)(d).io.instructionFetch :<>= uncompresser(tid)(d).io.decoder

      /** Connect the decoder and reorder buffer */
      reorderBuffer(tid).io.decoders(d) :<>= decoders(tid)(d).io.reorderBuffer

      /** Connect the decoder and reservation station */
      reservationStation(tid)(d).io.decoder :<>=
        decoders(tid)(d).io.reservationStation

      /** Connect the decoder and register file */
      registerFile(tid).io.decoders(d) :<>= decoders(tid)(d).io.registerFile

      /** Connect the decoder and LSQ */
      loadStoreQueue(tid).io.decoders(d) :<>= decoders(tid)(d).io.loadStoreQueue

      /** Decoder and output collector */
      decoders(tid)(d).io.outputCollector := outputCollector.io.outputs(tid)

      /** Connect the decoder and LSQ */
      loadStoreQueue(tid).io.decoders(d) :<>= decoders(tid)(d).io.loadStoreQueue
    }

    /** Connect fetch and branch results */
    fetch(tid).io.collectedBranchAddresses :=
      branchAddressCollector.io.fetch(tid)

    /** LSQ and output collector */
    loadStoreQueue(tid).io.outputCollector := outputCollector.io.outputs(tid)

    /** Register file and reorder buffer */
    registerFile(tid).io.reorderBuffer := reorderBuffer(tid).io.registerFile

    /** Connect fetch and the LSQ */
    fetch(tid).io.loadStoreQueueEmpty := loadStoreQueue(tid).io.empty

    /** Connect fetch and the reorder buffer */
    fetch(tid).io.reorderBufferEmpty := reorderBuffer(tid).io.isEmpty

    /** Data memory buffer and LSQ */
    (dataMemoryBuffer.io.dataIn(tid): ReadyValidIO[LoadStoreQueue2Memory]) :<>=
      (loadStoreQueue(tid).io.memory: ReadyValidIO[LoadStoreQueue2Memory])

    /** Reorder buffer and output collector */
    reorderBuffer(tid).io.collectedOutputs := outputCollector.io.outputs(tid)

    /** Reorder buffer and LSQ */
    loadStoreQueue(tid).io.reorderBuffer := reorderBuffer(tid).io.loadStoreQueue

    /** Connect instruction memory and the instruction cache */
    externalMemoryInterface.io.instruction(tid) :<>=
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

    instructionCache(tid).io.flush := fetch(tid).io.icache_flush

    /** Fetch and branch prediction TODO */
    fetch(tid).io.prediction.foreach(_.prediction := DontCare)
  }

  /** Memory and data memory buffer */
  externalMemoryInterface.io.data :<>= dataMemoryBuffer.io.memory
  outputCollector.io.dataMemory :<>= dataMemoryBuffer.io.output
  externalMemoryInterface.io.amo :<>= amo.io.memory
}

class B4SMTCoreFixedPorts(implicit params: Parameters) extends RawModule {
  override val desiredName = "B4SMTCore"
  val AXI_MM = IO(new VerilogAXI(64, 64))
  val axi = AXI_MM.viewAs[ChiselAXI]

  val aclk = IO(Input(Bool()))
  val aresetn = IO(Input(Bool()))

  withClockAndReset(aclk.asClock, (!aresetn).asAsyncReset) {
    val core = Module(new B4SMTCore())
    axi :<>= core.axi
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
      "--export-module-hierarchy",
      "--disable-all-randomization",
      "--add-vivado-ram-address-conflict-synthesis-bug-workaround",
    ),
  )
}
