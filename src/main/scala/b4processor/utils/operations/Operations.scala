package b4processor.utils.operations

import b4processor.riscv.Instructions
import b4processor.riscv.Instructions.{I64Type, ZICSRType, ZIFENCEIType, VType}
import b4processor.utils.BundleInitialize.AddBundleInitializeConstructor
import b4processor.utils.RVRegister
import b4processor.utils.RVRegister.{AddRegConstructor, AddUIntRegConstructor}
import chisel3._
import circt.stage.ChiselStage

import scala.language.implicitConversions

class Operations extends Bundle {
  val valid = Bool()
  val rs1 = new RVRegister
  val rs1Value = UInt(64.W)
  val rs1ValueValid = Bool()
  val rs2 = new RVRegister
  val rs2Value = UInt(64.W)
  val rs2ValueValid = Bool()
  val rd = new RVRegister
  val useRs2AsStoreSrc = Bool()
  val branchOffset = SInt(12.W)
  val aluOp = ALUOperation()
  val loadStoreOp = LoadStoreOperation()
  val loadStoreWidth = LoadStoreWidth()
  val fence = Bool()
  val ecall = Bool()
  val ebreak = Bool()
  val fence_i = Bool()
  val compressed = Bool()
  val csrOp = CSROperation()
  val csrAddress = UInt(12.W)
}

object Operations {
  implicit def Int2UInt(n: Int): UInt = n.U
  implicit def functionReduceSecondArgument(
    f: Operations => (Data, Data)
  ): (Operations, UInt) => (Data, Data) = (op, _) => f(op)

  private def createOperation(
    elems: ((Operations, UInt) => (Data, Data))*
  ): (UInt, UInt) => Operations = (instruction, pc) => {
    createOperationWithPC(elems.map {
      f => (op: Operations, inst: UInt, _pc: UInt) => f(op, inst)
    }: _*)(instruction, pc)
  }

  private def createOperationWithPC(
    elems: ((Operations, UInt, UInt) => (Data, Data))*
  ): (UInt, UInt) => Operations = (instruction, pc) => {
    val w = Wire(new Operations())
    w := DontCare
    w.valid := true.B
    for (e <- elems) {
      val a = e(w, instruction, pc)
      a._1 := a._2
    }
    w
  }

  def default: Operations = new Operations().initialize(
    _.valid -> false.B,
    _.rs1 -> 0.reg,
    _.rs2 -> 0.reg,
    _.rd -> 0.reg,
    _.useRs2AsStoreSrc -> false.B,
    _.rs1Value -> 0.U,
    _.rs2Value -> 0.U,
    _.rs1ValueValid -> false.B,
    _.rs2ValueValid -> false.B,
    _.aluOp -> ALUOperation.None,
    _.loadStoreOp -> LoadStoreOperation.None,
    _.loadStoreWidth -> LoadStoreWidth.Byte,
    _.branchOffset -> 0.S,
    _.fence -> false.B,
    _.fence_i -> false.B,
    _.ecall -> false.B,
    _.ebreak -> false.B,
    _.compressed -> false.B,
    _.csrOp -> CSROperation.None,
    _.csrAddress -> 0.U
  )

  implicit class UIntAccess(u: UInt) {
    def catAccess(t: (Int, Int)*): UInt =
      t.map(a => u(a._1, a._2)).reduce(_ ## _)
  }

  implicit def Int2IntTuple(n: Int): (Int, Int) = (n, n)

  def btypeOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> op,
      _.rs1 -> _(19, 15).reg,
      _.rs2 -> _(24, 20).reg,
      _.branchOffset -> _.catAccess(31, 7, (30, 25), (11, 8)).asSInt
    )
  def rtypeOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> op,
      _.rs1 -> _(19, 15).reg,
      _.rs2 -> _(24, 20).reg,
      _.rd -> _(11, 7).reg
    )

  def itypeOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> op,
      _.rs1 -> _(19, 15).reg,
      (u, _) => u.rs2 -> 0.reg,
      (a, b) => a.rs2Value -> b(31, 20).asSInt.pad(64).asUInt,
      (u, _) => u.rs2ValueValid -> true.B,
      _.rd -> _(11, 7).reg
    )

  def itype64ShiftOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> op,
      _.rs1 -> _(19, 15).reg,
      (u, _) => u.rs2 -> 0.reg,
      _.rs2Value -> _(25, 20),
      (u, _) => u.rs2ValueValid -> true.B,
      _.rd -> _(11, 7).reg
    )

  def itype64ShiftWOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> op,
      _.rs1 -> _(19, 15).reg,
      (u, _) => u.rs2 -> 0.reg,
      _.rs2Value -> _(24, 20),
      (u, _) => u.rs2ValueValid -> true.B,
      _.rd -> _(11, 7).reg
    )

  def utypeOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> op,
      _.rs1 -> _(19, 15).reg,
      (u, _) => u.rs2 -> 0.reg,
      _.rs2Value -> _(31, 20),
      (u, _) => u.rs2ValueValid -> true.B,
      _.rd -> _(11, 7).reg
    )

  def loadOp(
    op: LoadStoreOperation.Type,
    width: LoadStoreWidth.Type
  ): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> ALUOperation.None,
      (u, _) => u.loadStoreOp -> op,
      (u, _) => u.loadStoreWidth -> width,
      _.rs1 -> _(19, 15).reg,
      _.rs2Value -> _(31, 20).asSInt.pad(64).asUInt,
      (u, _) => u.rs2ValueValid -> true.B,
      _.rd -> _(11, 7).reg
    )

  def storeOp(
    op: LoadStoreOperation.Type,
    width: LoadStoreWidth.Type
  ): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> ALUOperation.None,
      (u, _) => u.loadStoreOp -> op,
      (u, _) => u.loadStoreWidth -> width,
      _.rs1 -> _(19, 15).reg,
      _.rs2Value -> _.catAccess((31, 25), (11, 7)).asSInt.pad(64).asUInt,
      (u, _) => u.rs2ValueValid -> true.B,
      (u, _) => u.rd -> 0.reg,
      _.rs2 -> _(24, 20).reg,
      (u, _) => u.useRs2AsStoreSrc -> true.B
    )

  def csrOp(op: CSROperation.Type): (UInt, UInt) => Operations =
    createOperation(
      _.rd -> _(11, 7).reg,
      _.rs1 -> _(19, 15).reg,
      _.rs2Value -> _(31, 20),
      (u, _) => u.rs2ValueValid -> true.B,
      (u, _) => u.csrOp -> op
    )

  def csrImmOp(op: CSROperation.Type): (UInt, UInt) => Operations =
    createOperation(
      _.rd -> _(11, 7).reg,
      _.rs1Value -> _(19, 15),
      (u, _) => u.rs1ValueValid -> true.B,
      _.rs2Value -> _(31, 20),
      (u, _) => u.rs2ValueValid -> true.B,
      (u, _) => u.csrOp -> op
    )

  def vsetvliOp(op: CSROperation.Type): (UInt, UInt) => Operations =
    createOperation(
      _.rd -> _(11,7).reg,
      _.rs1 -> _(19, 15).reg,
      _.rs2Value -> _(30, 20),
      (u, _) => u.rs2ValueValid -> true.B,
      (u, _) => u.csrOp -> op,
    )

  def vsetivliOp(op: CSROperation.Type): (UInt, UInt) => Operations =
    createOperation(
      _.rd -> _(11, 7).reg,
      _.rs1Value -> _(19, 15),
      (u, _) => u.rs1ValueValid -> true.B,
      _.rs2Value -> _(29, 20),
      (u, _) => u.rs2ValueValid -> true.B,
      (u, _) => u.csrOp -> op,
    )

  def vsetvlOp(op: CSROperation.Type): (UInt, UInt) => Operations =
    createOperation(
      _.rd -> _(11,7).reg,
      _.rs1 -> _(19,15).reg,
      _.rs2 -> _(24,20).reg,
      (u, _) => u.csrOp -> op
    )

  def decodingList = {
    import Instructions.IType
    Seq(
      IType("BEQ") -> btypeOp(ALUOperation.BranchEqual),
      IType("BNE") -> btypeOp(ALUOperation.BranchNotEqual),
      IType("BLT") -> btypeOp(ALUOperation.BranchLessThan),
      IType("BGE") -> btypeOp(ALUOperation.BranchGreaterThanOrEqual),
      IType("BLTU") -> btypeOp(ALUOperation.BranchLessThanUnsigned),
      IType("BGEU") -> btypeOp(ALUOperation.BranchGreaterThanOrEqualUnsigned),
      IType("ADD") -> rtypeOp(ALUOperation.Add),
      IType("SLT") -> rtypeOp(ALUOperation.Slt),
      IType("SLTU") -> rtypeOp(ALUOperation.Sltu),
      IType("AND") -> rtypeOp(ALUOperation.And),
      IType("OR") -> rtypeOp(ALUOperation.Or),
      IType("XOR") -> rtypeOp(ALUOperation.Xor),
      IType("SLL") -> rtypeOp(ALUOperation.Sll),
      IType("SRL") -> rtypeOp(ALUOperation.Srl),
      IType("SUB") -> rtypeOp(ALUOperation.Sub),
      IType("SRA") -> rtypeOp(ALUOperation.Sra),
      IType("ADDI") -> itypeOp(ALUOperation.Add),
      IType("SLTI") -> itypeOp(ALUOperation.Slt),
      IType("SLTIU") -> itypeOp(ALUOperation.Sltu),
      IType("ANDI") -> itypeOp(ALUOperation.And),
      IType("ORI") -> itypeOp(ALUOperation.Or),
      IType("XORI") -> itypeOp(ALUOperation.Xor),
      I64Type("SLLI") -> itypeOp(ALUOperation.Sll),
      I64Type("SRLI") -> itypeOp(ALUOperation.Srl),
      I64Type("SRAI") -> itypeOp(ALUOperation.Sra),
      IType("LUI") -> createOperation(
        _.rd -> _(11, 7).reg,
        (u, _) => u.rs1 -> 0.reg,
        (u, inst) =>
          u.rs1Value ->
            (inst(31, 12) ## 0.U(12.W)).asSInt
              .pad(64)
              .asUInt,
        (u, _) => u.rs1ValueValid -> true.B,
        (u, _) => u.rs2ValueValid -> true.B,
        (u, _) => u.rs2Value -> 0.U,
        (u, _) => u.aluOp -> ALUOperation.Add
      ),
      IType("AUIPC") -> createOperationWithPC(
        (u, inst, _) => u.rd -> inst(11, 7).reg,
        (u, inst, _) =>
          u.rs1Value ->
            (inst(31, 12) ## 0.U(12.W)).asSInt
              .pad(64)
              .asUInt,
        (u, _, _) => u.rs1ValueValid -> true.B,
        (u, _, _) => u.rs2ValueValid -> true.B,
        (u, _, pc) => u.rs2Value -> pc,
        (u, _, _) => u.aluOp -> ALUOperation.Add
      ),
      IType("JAL") -> createOperationWithPC(
        (u, inst, _) => u.rd -> inst(11, 7).reg,
        (u, _, pc) => u.rs2Value -> pc,
        (u, inst, _) =>
          u.rs1Value ->
            (inst.catAccess(31, (19, 12), 20, (30, 21)) ## 0.U(1.W)).asSInt
              .pad(64)
              .asUInt,
        (u, _, _) => u.rs1ValueValid -> true.B,
        (u, _, _) => u.rs2ValueValid -> true.B,
        (u, _, _) => u.aluOp -> ALUOperation.AddJAL
      ),
      IType("JALR") -> createOperationWithPC(
        (u, inst, _) => u.rd -> inst(11, 7).reg,
        (u, _, pc) => u.rs2Value -> pc,
        (u, inst, _) => u.rs1 -> inst(19, 15).reg,
        (u, _, _) => u.rs2ValueValid -> true.B,
        (u, inst, _) => u.branchOffset -> inst(31, 20).asSInt,
        (u, _, _) => u.aluOp -> ALUOperation.AddJALR
      ),
      IType("LB") -> loadOp(LoadStoreOperation.Load, LoadStoreWidth.Byte),
      IType("LBU") -> loadOp(
        LoadStoreOperation.LoadUnsigned,
        LoadStoreWidth.Byte
      ),
      IType("LH") -> loadOp(LoadStoreOperation.Load, LoadStoreWidth.HalfWord),
      IType("LHU") -> loadOp(
        LoadStoreOperation.LoadUnsigned,
        LoadStoreWidth.HalfWord
      ),
      IType("LW") -> loadOp(LoadStoreOperation.Load, LoadStoreWidth.Word),
      I64Type("LWU") -> loadOp(
        LoadStoreOperation.LoadUnsigned,
        LoadStoreWidth.Word
      ),
      I64Type("LD") -> loadOp(
        LoadStoreOperation.Load,
        LoadStoreWidth.DoubleWord
      ),
      IType("SB") -> storeOp(LoadStoreOperation.Store, LoadStoreWidth.Byte),
      IType("SH") -> storeOp(LoadStoreOperation.Store, LoadStoreWidth.HalfWord),
      IType("SW") -> storeOp(LoadStoreOperation.Store, LoadStoreWidth.Word),
      I64Type("SD") -> storeOp(
        LoadStoreOperation.Store,
        LoadStoreWidth.DoubleWord
      ),
      IType("FENCE") -> createOperation((u, _) => u.fence -> true.B),
      IType("ECALL") -> createOperation((u, _) => u.ecall -> true.B),
      IType("EBREAK") -> createOperation((u, _) => u.ebreak -> true.B),
      ZIFENCEIType("FENCE_I") -> createOperation((u, _) => u.fence_i -> true.B),
      I64Type("ADDIW") -> itypeOp(ALUOperation.AddW),
      I64Type("SLLI") -> itype64ShiftOp(ALUOperation.Sll),
      I64Type("SRLI") -> itype64ShiftOp(ALUOperation.Srl),
      I64Type("SRAI") -> itype64ShiftOp(ALUOperation.Sra),
      I64Type("SLLIW") -> itype64ShiftWOp(ALUOperation.SllW),
      I64Type("SRLIW") -> itype64ShiftWOp(ALUOperation.SrlW),
      I64Type("SRAIW") -> itype64ShiftWOp(ALUOperation.SraW),
      I64Type("ADDW") -> rtypeOp(ALUOperation.AddW),
      I64Type("SLLW") -> rtypeOp(ALUOperation.SllW),
      I64Type("SRLW") -> rtypeOp(ALUOperation.SrlW),
      I64Type("SUBW") -> rtypeOp(ALUOperation.SubW),
      I64Type("SRAW") -> rtypeOp(ALUOperation.SraW),
      ZICSRType("CSRRC") -> csrOp(CSROperation.ReadClear),
      ZICSRType("CSRRS") -> csrOp(CSROperation.ReadSet),
      ZICSRType("CSRRW") -> csrOp(CSROperation.ReadWrite),
      ZICSRType("CSRRCI") -> csrImmOp(CSROperation.ReadClear),
      ZICSRType("CSRRSI") -> csrImmOp(CSROperation.ReadSet),
      ZICSRType("CSRRWI") -> csrImmOp(CSROperation.ReadWrite),
      VType("VSETVLI") -> vsetvliOp(CSROperation.SetVl),
      VType("VSETIVLI") -> vsetivliOp(CSROperation.SetVl),
      VType("VSETVL") -> vsetvlOp(CSROperation.SetVl),
    )
  }

  def genDecoder(inst: UInt, pc: UInt): Operations = {
    var output = Operations.default
    for (d <- decodingList) {
      output = Mux(d._1 === inst, d._2(inst, pc), output)
    }
    output
  }
}

class DecodingMod extends Module {
  val input = IO(Input(UInt(32.W)))
  val pc = IO(Input(UInt(64.W)))
  val out = IO(Output(new Operations()))
  out := Operations.genDecoder(input, pc)
}

object DecodingMod {
  def apply(instruction: UInt, programCounter: UInt): Operations = {
    val m = Module(new DecodingMod())
    m.input := instruction
    m.pc := programCounter
    m.out
  }
}

object OperationDecoderApp extends App {
  ChiselStage.emitSystemVerilogFile(new DecodingMod)
}

object ALUOperation extends ChiselEnum {
  val None, BranchEqual, BranchNotEqual, BranchLessThan,
    BranchGreaterThanOrEqual, BranchLessThanUnsigned,
    BranchGreaterThanOrEqualUnsigned, Add, Sub, And, Or, Slt, Sltu, Xor, Sll,
    Srl, Sra, AddJALR, AddJAL, AddW, SllW, SrlW, SraW, SubW = Value
}

object LoadStoreOperation extends ChiselEnum {
  val None, Load, LoadUnsigned, Store = Value
}

object LoadStoreWidth extends ChiselEnum {
  val Byte, HalfWord, Word, DoubleWord = Value
}

object CSROperation extends ChiselEnum {
  val None, ReadWrite, ReadSet, ReadClear, SetVl = Value
}
