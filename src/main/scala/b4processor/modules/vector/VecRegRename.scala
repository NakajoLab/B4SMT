package b4processor.modules.vector

import b4processor._
import chisel3._
import chisel3.util._

class VecRegRename(implicit params: Parameters) extends Module {
  val physicalVecRegIdxLen = log2Up(params.physicalVrfEntries)
  val vrfPrefix = log2Up(params.threads)
  val io = IO(new Bundle {
    val vs1 = Input(Vec(params.threads, UInt(5.W)))
    val vs2 = Input(Vec(params.threads, UInt(5.W)))
    val vd = Input(Vec(params.threads, UInt(5.W)))
    val writeToVd = Input(Vec(params.threads, Bool()))
    // 0: vop v10 -> pv53, ...
    // 1: vop v10 -> pv54, v10 -> pv53, ...
    // で命令1がリタイアしたとき，v10が上書きされるので先行する命令0の結果は上書きされる．
    // 従って該当の際に投機状態テーブルのv10が指す物理レジスタpv53をフリーリストに入れる
    // またリタイア状態テーブルのv10が指す物理レジスタをpv53に更新する？
    val deAllocatedVd = Input(Vec(params.threads, Valid(UInt(5.W))))
    val renamedVs1 = Output(Vec(params.threads, UInt(physicalVecRegIdxLen.W)))
    val renamedVs2 = Output(Vec(params.threads, UInt(physicalVecRegIdxLen.W)))
    val renamedVdAsSource = Output(Vec(params.threads, UInt(physicalVecRegIdxLen.W)))
    // vdのリネームでは書き込む場合にフリーリストから取ってきたものを使う
    val renamedVdAsDest = Output(Vec(params.threads, UInt(physicalVecRegIdxLen.W)))
  })

  // 32 * スレッド数 の論理ベクトルレジスタを物理レジスタ番号に対応させる
  // 2スレッドデフォルト: (0, 1, 2, ..., 62, 63)
  val speculativeTable = RegInit(VecInit(
    (0 until 32 * params.threads).map(
      i => i.U(physicalVecRegIdxLen.W)
    )
  ))
  // 利用可能な物理レジスタ番号
  // 2スレッドデフォルト: (64, 65, ..., 95)
  val freeList = RegInit(VecInit(
    (0 until params.physicalVrfEntries).map(
      i => (if(i < (params.physicalVrfFor1Thread * 16)) i + (32 * params.threads) else 0).U
    )
  ))
  val freeListHead = RegInit(0.U)
  val freeListTail = RegInit((params.physicalVrfFor1Thread * 16).U)

  for(((out, in), i) <- (io.renamedVs1 zip io.vs1).zipWithIndex) {
    out := speculativeTable(Cat(i.U(vrfPrefix.W), speculativeTable(in)))
  }
  for(((out, in), i) <- (io.renamedVs2 zip io.vs2).zipWithIndex) {
    out := speculativeTable(Cat(i.U(vrfPrefix.W), speculativeTable(in)))
  }
  for(((out, in), i) <- (io.renamedVdAsSource zip io.vd).zipWithIndex) {
    out := speculativeTable(Cat(i.U(vrfPrefix.W), speculativeTable(in)))
  }
  // スレッドiがvdへ書き込む -> スレッド0からi-1までのwriteToVdのsum + freeListHead, インデックスが超える場合は - params.physicalVrfEntries
  for(((out, in), i) <- (io.renamedVdAsDest zip io.writeToVd).zipWithIndex) {
    when(in) {
      val writeToVdSum = i match {
        case 0 => 0.U
        case 1 => io.writeToVd(0).asUInt
        case _ => io.writeToVd.slice(0, i).map(_.asUInt).reduce(_ +& _)
      }
      val freeListReadIdx = freeListHead +& writeToVdSum
      out := freeList(Mux(freeListReadIdx >= params.physicalVrfEntries.U,
        freeListReadIdx - params.physicalVrfEntries.U,
        freeListReadIdx
      ))
      speculativeTable(Cat(i.U(vrfPrefix.W), io.vd(i))) := out
    }
  }
  val freeListHeadIncNum = io.writeToVd.map(_.asUInt).reduce(_ +& _)
  freeListHead := Mux(freeListHead + freeListHeadIncNum >= params.physicalVrfEntries.U, freeListHead + freeListHeadIncNum - params.physicalVrfEntries.U, freeListHead + freeListHeadIncNum)

  // freeListへの使用済みインデックス入力

}
