package b4smt.modules.registerfile

import circt.stage.ChiselStage
import b4smt.Parameters
import b4smt.connections.{Decoder2RegisterFile, ReorderBuffer2RegisterFile}
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._

/** Register file
  *
  * @param params
  *   Parameters
  */
class RegisterFile(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {

    /** To the decoder */
    val decoders =
      Flipped(Vec(params.decoderPerThread, new Decoder2RegisterFile))

    /** Reorder buffer */
    val reorderBuffer = Flipped(
      Vec(
        params.maxRegisterFileCommitCount,
        Valid(new ReorderBuffer2RegisterFile()),
      ),
    )

    val threadId = Input(UInt(log2Up(params.threads).W))

    val values = if (params.debug) Some(Output(Vec(32, UInt(64.W)))) else None
  })

  /** Registers x1~x31; only tp is initialized with threadId
    */
//  val registers = RegInit(VecInit(Seq.fill(32)(-1.S(64.W).asUInt)))
  val registers = Reg(Vec(32, UInt(64.W)))
//  val registers = Mem(32, UInt(64.W))

  for ((rb, i) <- io.reorderBuffer.zipWithIndex) {
    prefix(s"in${i}") {
      val valid = rb.valid && rb.bits.destinationRegister.inner =/= 0.U
      when(valid) {
        registers(rb.bits.destinationRegister.inner) := rb.bits.value
      }
    }
  }

  // Signals to each decoder
  for (dec <- io.decoders) {
    // Use 0 if the source register is 0; otherwise read from the register
    dec.values zip dec.sourceRegisters foreach { case (v, s) =>
      v := registers(s.inner)
    }
  }

  registers(0) := 0.U

  // Debug signals
  if (params.debug) {
    for (i <- 0 until 32)
      io.values.get(i) := registers(i)
  }
}

object RegisterFile extends App {
  implicit val params: b4smt.Parameters = Parameters()
  ChiselStage.emitSystemVerilogFile(
    new RegisterFile,
    Array(),
    Array("--disable-all-randomization"),
  )
}
