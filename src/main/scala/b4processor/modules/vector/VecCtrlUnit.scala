package b4processor.modules.vector


import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import b4processor._

class VtypeBundle(implicit params: Parameters) extends Bundle {
  val vill = Bool()
  val vma = Bool()
  val vta = Bool()
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
  def getBits: UInt = Mux(vill, Cat(true.B, 0.U((params.xprlen-1).W)), Cat(0.U((params.xprlen-8).W), vma, vta, vsew, vlmul))
  def setBits(vtypei: UInt): Unit = {
    vill := vtypei(params.xprlen-1) || (vtypei(params.xprlen-2, 8) =/= 0.U) || vtypei(5) || (vtypei(2,0) =/= 0.U)
    vma := vtypei(7)
    vta := vtypei(6)
    vsew := vtypei(5,3)
    vlmul := vtypei(2,0)
  }
}

class VecCtrlUnitReq(implicit params: Parameters) extends Bundle {
  val vDecode = new VectorDecoderResp
  val rs1_value = UInt(params.xprlen.W)
  val rs2_value = UInt(params.xprlen.W)
  val zimm = UInt(params.xprlen.W)
  val uimm = UInt(params.xprlen.W)
}

class VecCtrlUnitResp(implicit params: Parameters) extends Bundle {
  val vtype = new VtypeBundle()
  val vl = UInt(log2Up(params.vlenb).W)
}

class VecCtrlUnitIO(implicit params: Parameters) extends Bundle {
  val req = Input(Valid(new VecCtrlUnitReq))
  val resp = Output(Valid(new VecCtrlUnitResp()))
}

class VecCtrlUnit(implicit params: Parameters) extends Module with VectorOpConstants {
  val io = IO(new VecCtrlUnitIO())

  val avl = Mux(io.req.bits.vDecode.avl_sel === AVL_SEL.RS1.asUInt, io.req.bits.rs1_value, io.req.bits.uimm)
  val vtypeBits = Wire(new VtypeBundle())
  vtypeBits.setBits(MuxLookup(io.req.bits.vDecode.vtype_sel, 0.U)(Seq(
    VTYPE_SEL.ZIMM10.asUInt -> Cat(false.B, io.req.bits.zimm(10,0)),
    VTYPE_SEL.ZIMM9.asUInt -> Cat(false.B, io.req.bits.zimm(9,0)),
    VTYPE_SEL.RS2.asUInt -> io.req.bits.rs2_value
  )))
  // val usedVectorBits = MuxLookup(vtypeBits.vsew, 0.U)(
  //   (0 until 4).map(i => i.U(3.W) -> (avl << (3+i).U).asUInt)
  // )
  val maxVl = MuxLookup(vtypeBits.vsew, 0.U)(
    // 0 -> Byte (8bits) -> params.vlen / 8 -> params.vlen >> 3
    // 1 -> Halfword (16bits) -> params.vlen / 16
    (0 until 4).map(i => i.U(3.W) -> (params.vlen >> (3+i)).U)
  )

  when(io.req.valid) {
    io.resp.bits.vtype := vtypeBits
    // if avl >= maxVl, then vl = maxVl, else vl = avl
    io.resp.bits.vl := Mux(avl >= maxVl, maxVl, avl)
    io.resp.valid := true.B
  } .otherwise {
    io.resp := DontCare
    io.resp.valid := false.B
  }
}

object VecCtrlUnit extends App {
  def apply(implicit params: HajimeCoreParams): VecCtrlUnit = new VecCtrlUnit()
  ChiselStage.emitSystemVerilogFile(VecCtrlUnit(HajimeCoreParams()), firtoolOpts = COMPILE_CONSTANTS.FIRTOOLOPS)
}