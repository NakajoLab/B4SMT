package b4processor.modules.decoder

import b4processor.connections.{FetchBuffer2Uncompresser, Uncompresser2Decoder}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

object CExtension {
  def ADDI4SPN: BitPat = BitPat("b000")
}

object FormatI {
  def apply(operation: (UInt, UInt), rd: UInt, rs1: UInt, imm: UInt): UInt = {
    require(operation._1.getWidth == 7)
    require(operation._2.getWidth == 3)
    require(rs1.getWidth == 5)
    require(rd.getWidth == 5)
    require(imm.getWidth == 12)
    val output = imm ## rs1 ## operation._2 ## rd ## operation._1
    require(output.getWidth == 32)
    output
  }

  def apply(
    operation: (UInt, UInt, UInt),
    rd: UInt,
    rs1: UInt,
    imm: UInt
  ): UInt = {
    require(operation._1.getWidth == 7)
    require(operation._2.getWidth == 3)
    require(operation._3.getWidth == 6)
    require(rs1.getWidth == 5)
    require(rd.getWidth == 5)
    require(imm.getWidth == 6)
    val output =
      operation._3 ## imm ## rs1 ## operation._2 ## rd ## operation._1
    require(output.getWidth == 32)
    output
  }
  def addi = ("b0010011".U(7.W), "b000".U(3.W))
  def addiw = ("b0011011".U(7.W), "b000".U(3.W))
  def srai = ("b0010011".U(7.W), "b101".U(3.W), "b010000".U(6.W))
  def srli = ("b0010011".U(7.W), "b101".U(3.W), "b000000".U(6.W))
  def slli = ("b0010011".U(7.W), "b001".U(3.W), "b000000".U(6.W))
  def andi = ("b0010011".U(7.W), "b111".U(3.W))
  def lw = ("b0000011".U(7.W), "b010".U(3.W))
  def ld = ("b0000011".U(7.W), "b011".U(3.W))
  def jalr = ("b1100111".U(7.W), "b000".U(3.W))
}

object FormatS {
  def apply(operation: (UInt, UInt), rs1: UInt, rs2: UInt, imm: UInt): UInt = {
    require(operation._1.getWidth == 7)
    require(operation._2.getWidth == 3)
    require(rs1.getWidth == 5)
    require(rs2.getWidth == 5)
    require(imm.getWidth == 12)
    val output =
      imm(11, 5) ## rs2 ## rs1 ## operation._2 ## imm(4, 0) ## operation._1
    require(output.getWidth == 32)
    output
  }

  def sw = ("b0100011".U(7.W), "b010".U(3.W))
  def sd = ("b0100011".U(7.W), "b011".U(3.W))
}

object FormatB {
  def apply(
    operation: (UInt, UInt),
    src1: UInt,
    src2: UInt,
    offset: UInt
  ): UInt = {
    require(operation._1.getWidth == 7)
    require(operation._2.getWidth == 3)
    require(src1.getWidth == 5)
    require(src2.getWidth == 5)
    require(offset.getWidth == 13)
    val output = offset(12) ## offset(10, 5) ## src2 ##
      src1 ## operation._2 ## offset(4, 1) ##
      offset(11) ## operation._1
    require(output.getWidth == 32)
    output
  }

  def beq = ("b1100011".U(7.W), "b000".U(3.W))
  def bne = ("b1100011".U(7.W), "b001".U(3.W))
}

object FormatJ {
  def apply(opcode: UInt, rd: UInt, imm: UInt): UInt = {
    require(rd.getWidth == 5)
    require(opcode.getWidth == 7)
    require(imm.getWidth == 22)
    val output = Cat(imm(20), imm(10, 1), imm(11), imm(19, 12), rd, opcode)
    require(output.getWidth == 32)
    output
  }

  def jal = "b1101111".U(7.W)
}

object FormatU {
  def apply(opcode: UInt, rd: UInt, imm: UInt): UInt = {
    require(opcode.getWidth == 7)
    require(rd.getWidth == 5)
    require(imm.getWidth == 20)
    val output = imm ## rd ## opcode
    require(output.getWidth == 32)
    output
  }

  def lui = "b0110111".U(7.W)
}

object FormatR {
  def apply(
    operation: (UInt, UInt, UInt),
    rd: UInt,
    rs1: UInt,
    rs2: UInt
  ): UInt = {
    require(operation._1.getWidth == 7)
    require(operation._2.getWidth == 3)
    require(operation._3.getWidth == 7)
    require(rd.getWidth == 5)
    require(rs1.getWidth == 5)
    require(rs2.getWidth == 5)
    val output =
      operation._3 ## rs2 ## rs1 ## operation._2 ## rd ## operation._1
    require(output.getWidth == 32)
    output
  }

  def add = ("b0110011".U(7.W), "b000".U(3.W), "b0000000".U(7.W))
  def sub = ("b0110011".U(7.W), "b000".U(3.W), "b0100000".U(7.W))
  def xor = ("b0110011".U(7.W), "b100".U(3.W), "b0000000".U(7.W))
  def or = ("b0110011".U(7.W), "b110".U(3.W), "b0000000".U(7.W))
  def and = ("b0110011".U(7.W), "b111".U(3.W), "b0000000".U(7.W))
  def addw = ("b0111011".U(7.W), "b000".U(3.W), "b0000000".U(7.W))
  def subw = ("b0111011".U(7.W), "b000".U(3.W), "b0100000".U(7.W))
}

object SignExtend {
  def apply(data: UInt, width: Int): UInt = {
    val w = Wire(UInt(width.W))
    w := Mux(
      !data(data.getWidth - 1),
      data,
      ~0.U((width - data.getWidth).W) ## data
    )
    w
  }
}

object ZeroExtend {
  def apply(data: UInt, width: Int): UInt = {
    val w = Wire(UInt(width.W))
    w := data
    w
  }
}

class Uncompresser extends Module {
  val io = IO(new Bundle {
    val fetch = Flipped(new FetchBuffer2Uncompresser())
    val decoder = new Uncompresser2Decoder()
  })

  io.fetch.ready := io.decoder.ready
  io.decoder.valid := io.fetch.valid
  io.decoder.bits.programCounter := io.fetch.bits.programCounter

  val instruction = io.fetch.bits.instruction

  io.decoder.bits.wasCompressed := instruction(1, 0) =/= "b11".U
  io.decoder.bits.instruction := MuxLookup(instruction(1, 0), 0.U)(
    Seq(
      "b00".U -> MuxLookup(instruction(15, 13), 0.U)(
        Seq(
          "b000".U -> Mux(
            instruction(12, 2) =/= 0.U,
            FormatI(
              FormatI.addi,
              1.U(2.W) ## instruction(4, 2),
              2.U(5.W),
              ZeroExtend(
                Cat(
                  instruction(10, 7),
                  instruction(12, 11),
                  instruction(5),
                  instruction(6),
                  0.U(2.W)
                ),
                12
              )
            ),
            0.U
          ),
          "b010".U -> FormatI(
            FormatI.lw,
            "b01".U(2.W) ## instruction(4, 2),
            "b01".U(2.W) ## instruction(9, 7),
            ZeroExtend(
              Cat(
                instruction(5),
                instruction(12, 10),
                instruction(6),
                0.U(2.W)
              ),
              12
            )
          ),
          "b011".U -> FormatI(
            FormatI.ld,
            "b01".U(2.W) ## instruction(4, 2),
            "b01".U(2.W) ## instruction(9, 7),
            ZeroExtend(
              Cat(instruction(6, 5), instruction(12, 10), 0.U(3.W)),
              12
            )
          ),
          "b110".U -> FormatS(
            FormatS.sw,
            "b01".U(2.W) ## instruction(9, 7),
            "b01".U(2.W) ## instruction(4, 2),
            ZeroExtend(
              Cat(
                instruction(5),
                instruction(12, 10),
                instruction(6),
                0.U(2.W)
              ),
              12
            )
          ),
          "b111".U -> FormatS(
            FormatS.sd,
            "b01".U(2.W) ## instruction(9, 7),
            "b01".U(2.W) ## instruction(4, 2),
            ZeroExtend(
              Cat(instruction(6, 5), instruction(12, 10), 0.U(3.W)),
              12
            )
          )
        )
      ),
      "b01".U -> MuxLookup(instruction(15, 13), 0.U)(
        Seq(
          "b000".U -> Mux(
            instruction(11, 7) === 0.U,
            FormatI(FormatI.addi, 0.U(5.W), 0.U(5.W), 0.U(12.W)),
            FormatI(
              FormatI.addi,
              instruction(11, 7),
              instruction(11, 7),
              SignExtend(instruction(12) ## instruction(6, 2), 12)
            )
          ),
          "b001".U -> Mux(
            instruction(11, 7) === 0.U,
            0.U,
            FormatI(
              FormatI.addiw,
              instruction(11, 7),
              instruction(11, 7),
              SignExtend(instruction(12) ## instruction(6, 2), 12)
            )
          ),
          "b010".U -> Mux(
            instruction(11, 7) === 0.U,
            0.U,
            FormatI(
              FormatI.addi,
              instruction(11, 7),
              0.U(5.W),
              SignExtend(instruction(12) ## instruction(6, 2), 12)
            )
          ),
          "b011".U -> MuxCase(
            0.U,
            Seq(
              (instruction(11, 7) === 2.U) -> FormatI(
                FormatI.addi,
                2.U(5.W),
                2.U(5.W),
                SignExtend(
                  Cat(
                    instruction(12),
                    instruction(4, 3),
                    instruction(5),
                    instruction(2),
                    instruction(6),
                    0.U(4.W)
                  ),
                  12
                )
              ),
              (instruction(11, 7) =/= 2.U) -> FormatU(
                FormatU.lui,
                instruction(11, 7),
                SignExtend(Cat(instruction(12), instruction(6, 2)), 20)
              )
            )
          ),
          "b100".U -> MuxLookup(instruction(11, 10), 0.U)(
            Seq(
              "b00".U -> FormatI(
                FormatI.srli,
                "b01".U(2.W) ## instruction(9, 7),
                "b01".U(2.W) ## instruction(9, 7),
                ZeroExtend(Cat(instruction(12), instruction(6, 2)), 6)
              ),
              "b01".U -> FormatI(
                FormatI.srai,
                "b01".U(2.W) ## instruction(9, 7),
                "b01".U(2.W) ## instruction(9, 7),
                ZeroExtend(Cat(instruction(12), instruction(6, 2)), 6)
              ),
              "b10".U -> FormatI(
                FormatI.andi,
                "b01".U(2.W) ## instruction(9, 7),
                "b01".U(2.W) ## instruction(9, 7),
                SignExtend(Cat(instruction(12), instruction(6, 2)), 12)
              ),
              "b11".U -> Mux(
                instruction(12),
                MuxLookup(instruction(6, 5), 0.U)(
                  Seq(
                    "b00".U -> FormatR(
                      FormatR.subw,
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(4, 2)
                    ),
                    "b01".U -> FormatR(
                      FormatR.addw,
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(4, 2)
                    )
                  )
                ),
                MuxLookup(instruction(6, 5), 0.U)(
                  Seq(
                    "b00".U -> FormatR(
                      FormatR.sub,
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(4, 2)
                    ),
                    "b01".U -> FormatR(
                      FormatR.xor,
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(4, 2)
                    ),
                    "b10".U -> FormatR(
                      FormatR.or,
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(4, 2)
                    ),
                    "b11".U -> FormatR(
                      FormatR.and,
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(9, 7),
                      "b01".U(2.W) ## instruction(4, 2)
                    )
                  )
                )
              )
            )
          ),
          "b101".U -> FormatJ(
            FormatJ.jal,
            0.U(5.W),
            SignExtend(
              Cat(
                instruction(12),
                instruction(8),
                instruction(10, 9),
                instruction(6),
                instruction(7),
                instruction(2),
                instruction(11),
                instruction(5, 3),
                0.U(1.W)
              ),
              22
            )
          ),
          "b110".U -> FormatB(
            FormatB.beq,
            1.U(2.W) ## instruction(9, 7),
            0.U(5.W),
            SignExtend(
              Cat(
                instruction(12),
                instruction(6, 5),
                instruction(2),
                instruction(11, 10),
                instruction(4, 3),
                0.U(1.W)
              ),
              13
            )
          ),
          "b111".U -> FormatB(
            FormatB.bne,
            1.U(2.W) ## instruction(9, 7),
            0.U(5.W),
            SignExtend(
              Cat(
                instruction(12),
                instruction(6, 5),
                instruction(2),
                instruction(11, 10),
                instruction(4, 3),
                0.U(1.W)
              ),
              13
            )
          )
        )
      ),
      "b10".U -> MuxLookup(instruction(15, 13), 0.U)(
        Seq(
          "b000".U -> FormatI(
            FormatI.slli,
            instruction(11, 7),
            instruction(11, 7),
            ZeroExtend(Cat(instruction(12), instruction(6, 2)), 6)
          ),
          "b010".U -> FormatI(
            FormatI.lw,
            instruction(11, 7),
            2.U(5.W),
            ZeroExtend(
              Cat(
                instruction(3, 2),
                instruction(12),
                instruction(6, 4),
                0.U(2.W)
              ),
              12
            )
          ),
          "b011".U -> FormatI(
            FormatI.ld,
            instruction(11, 7),
            2.U(5.W),
            ZeroExtend(
              Cat(
                instruction(4, 2),
                instruction(12),
                instruction(6, 5),
                0.U(3.W)
              ),
              12
            )
          ),
          "b100".U -> Mux(
            !instruction(12),
            Mux(
              instruction(6, 2) === 0.U,
              FormatI(FormatI.jalr, 0.U(5.W), instruction(11, 7), 0.U(12.W)),
              FormatI(
                FormatI.addi,
                instruction(11, 7),
                instruction(6, 2),
                0.U(12.W)
              )
            ),
            Mux(
              instruction(11, 7) === 0.U,
              0.U,
              Mux(
                instruction(6, 2) === 0.U,
                FormatI(FormatI.jalr, 1.U(5.W), instruction(11, 7), 0.U(12.W)),
                FormatR(
                  FormatR.add,
                  instruction(11, 7),
                  instruction(11, 7),
                  instruction(6, 2)
                )
              )
            )
          ),
          "b110".U -> FormatS( // swsp
            FormatS.sw,
            2.U(5.W),
            instruction(6, 2),
            ZeroExtend(Cat(instruction(8, 7), instruction(12, 9), 0.U(2.W)), 12)
          ),
          "b111".U -> FormatS( // sdsp
            FormatS.sd,
            2.U(5.W),
            instruction(6, 2),
            ZeroExtend(
              Cat(instruction(9, 7), instruction(12, 10), 0.U(3.W)),
              12
            )
          )
        )
      ),
      "b11".U -> instruction
    )
  )
}

object Uncompresser extends App {
  ChiselStage.emitSystemVerilogFile(new Uncompresser)
}
