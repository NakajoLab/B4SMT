package b4processor.modules.lsq

import b4processor.Parameters
import b4processor.connections.{
  CollectedOutput,
  Decoder2LoadStoreQueue,
  LoadStoreQueue2Memory,
  LoadStoreQueue2ReorderBuffer
}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4processor.utils.Tag
import b4processor.utils.operations.LoadStoreOperation

class LoadStoreQueue(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val decoders =
      Vec(
        params.decoderPerThread,
        Flipped(Decoupled(new Decoder2LoadStoreQueue()))
      )
    val outputCollector = Flipped(new CollectedOutput)
    val reorderBuffer = Flipped(
      Vec(
        params.maxRegisterFileCommitCount,
        Valid(new LoadStoreQueue2ReorderBuffer)
      )
    )
    val memory = Decoupled(new LoadStoreQueue2Memory)
    val isEmpty = Output(Bool())

    val head =
      if (params.debug) Some(Output(UInt(params.loadStoreQueueIndexWidth.W)))
      else None
    val tail =
      if (params.debug) Some(Output(UInt(params.loadStoreQueueIndexWidth.W)))
      else None
    // LSQのエントリ数はこのままでいいのか
  })

  val defaultEntry = LoadStoreQueueEntry.default

  val head = RegInit(0.U(params.loadStoreQueueIndexWidth.W))
  val tail = RegInit(0.U(params.loadStoreQueueIndexWidth.W))
  io.isEmpty := head === tail

  val buffer = RegInit(
    VecInit(
      Seq.fill(math.pow(2, params.loadStoreQueueIndexWidth).toInt)(defaultEntry)
    )
  )
  var insertIndex = head

  /** デコードした命令をLSQに加えるかどうか確認し，l or s 命令ならばエンキュー */
  for (i <- 0 until params.decoderPerThread) {
    val decoder = io.decoders(i)
    decoder.ready := tail =/= insertIndex + 1.U
    val entryValid = decoder.ready && decoder.valid

    /** 現状，(LSQの最大エントリ数 = リオーダバッファの最大エントリ数)であり，
      * プロセッサで同時実行可能な最大命令数がリオーダバッファのエントリ番号数(dtag数)であることから，
      * エンキュー時の命令待機は必要ないが，LSQのエントリ数を減らした場合，必要
      */
    when(entryValid) {
      //      printf("isLoad = %d\n", decoder.bits.opcode === LOAD)
      buffer(insertIndex) := LoadStoreQueueEntry.validEntry(
        // opcode = 1(load), 0(store) (bit数削減)
        operation = decoder.bits.operation,
        operationWidth = decoder.bits.operationWidth,
        destinationTag = decoder.bits.destinationTag,
        address = decoder.bits.address,
        addressValid = decoder.bits.addressValid,
        addressOffset = decoder.bits.addressOffset,
        addressTag = decoder.bits.addressTag,
        storeDataTag = decoder.bits.storeDataTag,
        storeData = decoder.bits.storeData,
        storeDataValid = decoder.bits.storeDataValid
      )
    }
    insertIndex = insertIndex + entryValid.asUInt
  }

  head := insertIndex

  /** オペランドバイパスのタグorPCが対応していた場合は，ALUを読み込む */

  private val output = io.outputCollector.outputs
  for (buf <- buffer) {
    for (o <- output) {
      when(o.valid && buf.valid) {
        when(buf.storeDataTag === o.bits.tag && !buf.storeDataValid) {
          buf.storeDataTag := Tag(0, 0)
          buf.storeData := o.bits.value
          buf.storeDataValid := true.B
        }
        when(buf.addressTag === o.bits.tag && !buf.addressValid) {
          buf.addressTag := Tag(0, 0)
          buf.address := o.bits.value
          buf.addressValid := true.B
        }
      }
    }
  }

  for (rb <- io.reorderBuffer) {
    when(rb.valid) {
      for (buf <- buffer) {
        when(buf.valid && (rb.bits.destinationTag === buf.destinationTag)) {
          buf.readyReorderSign := true.B
        }
      }
    }
  }

  /** Ra = 1 && R=0の先行するストア命令と実効アドレスが被っていなければ ロード命令をメモリに送出 送出後, R=1
    */

  /** Ra = Rd = 1 && リオーダバッファから命令送出信号が来れば ストア命令をメモリに送出 送出後, R=1
    */
  // Overlap : 先行する命令の実効アドレスとの被りがあるかのフラグ (T:あり　F:なし)
  // Address : 送出対象の命令のアドレスを格納
  val Overlap = WireInit(
    VecInit(Seq.fill(params.loadStoreQueueCheckLength)(false.B))
  )
  val Address = WireInit(
    VecInit(Seq.fill(params.loadStoreQueueCheckLength)(0.U(64.W)))
  )
  val AddressValid = WireInit(
    VecInit(Seq.fill(params.loadStoreQueueCheckLength)(false.B))
  )
  val EntryValid = WireInit(
    VecInit(Seq.fill(params.loadStoreQueueCheckLength)(false.B))
  )

  // emissionindex : 送出可能か調べるエントリを指すindex
  // nexttail      : 1クロック分の送出確認後，動かすtailのエントリを指すindex
  var nextTail = tail
  var isSet = false.B

  io.memory.bits := 0.U.asTypeOf(new LoadStoreQueue2Memory)
  io.memory.valid := false.B
  for (i <- 0 until params.loadStoreQueueCheckLength) {
    val checkIndex = tail + i.U
    val checkOk = WireDefault(false.B)
    when(buffer(checkIndex).valid) {
      val buf = buffer(checkIndex)
      Address(i) := (buf.address.asSInt + buf.addressOffset).asUInt
      AddressValid(i) := buf.addressValid
      EntryValid(i) := buf.valid
      //      Overlap(i) := false.B
      // 先行する命令が持つアドレスの中に被りがある場合，Overlap(i) := true.B
      if (i != 0) {
        for (j <- 0 until i) {
          when(EntryValid(j)) {
            when(
              (AddressValid(j) && Address(j) === Address(i)) || !AddressValid(j)
            ) {
              Overlap(i) := true.B
            }
          }
        }
      }

      val operationIsStore =
        LoadStoreOperation.Store === buf.operation

      // io.memory(i).valid :=  io.memory(i).ready && (head =/= tail) && ("loadの送出条件" || "storeの送出条件")
      checkOk := (head =/= tail) && buf.valid &&
        buf.addressValid &&
        ((!operationIsStore && !Overlap(i)) ||
          (operationIsStore && buf.storeDataValid &&
            buf.readyReorderSign))

      io.memory.valid := checkOk | isSet
      // 送出実行
      when(checkOk && !isSet) {
        io.memory.bits.tag := buf.destinationTag
        io.memory.bits.data := buf.storeData
        io.memory.bits.address := Address(i)
        io.memory.bits.operation := buf.operation
        io.memory.bits.operationWidth := buf.operationWidth
        when(io.memory.ready) {
          buf := LoadStoreQueueEntry.default
        }
      }
    }

    isSet = isSet | checkOk
  }
  when(!buffer(tail).valid && head =/= tail) {
    tail := tail + 1.U
  }

  // デバッグ
  if (params.debug) {
    io.head.get := head
    io.tail.get := tail
  }
}

object LoadStoreQueue extends App {
  implicit val params = Parameters(maxRegisterFileCommitCount = 2, tagWidth = 4)
  ChiselStage.emitSystemVerilogFile(new LoadStoreQueue)
}
