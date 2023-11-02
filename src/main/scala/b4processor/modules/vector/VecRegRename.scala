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
    // vdのリネームでは書き込む場合にフリーリストから取ってきたものを使う
    val vd = Flipped(ValidIO(UInt((5 + vrfPrefix).W)))
    // 0: vop v10 -> pv53, ...
    // 1: vop v10 -> pv54, v10 -> pv53, ...
    // で命令1がリタイアしたとき，v10が上書きされるので先行する命令0の結果は上書きされる．
    // 従って該当の際に投機状態テーブルのv10が指す物理レジスタpv53をフリーリストに入れる
    // またリタイア状態テーブルのv10が指す物理レジスタをpv53に更新する？
    val deAllocatedVd = Flipped(ValidIO(UInt(5.W)))
    val renamedVs1 = Output(Vec(params.threads, UInt(physicalVecRegIdxLen.W)))
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
      i => i.U + (32 * params.threads)
    )
  ))
  for(((out, in), i) <- (io.renamedVs1 zip io.vs1).zipWithIndex) {
    out := speculativeTable(Cat(i.U(vrfPrefix.W), speculativeTable(in)))
  }
}
