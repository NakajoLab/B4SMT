package b4smt.modules.registerfile

import b4smt.Parameters
import b4smt.connections.{Decoder2RegisterFile, ReorderBuffer2RegisterFile}
import b4smt.utils.RVRegister.AddRegConstructor
import chisel3._
import chisel3.experimental.prefix
import circt.stage.ChiselStage
import chisel3.util._

/** Register file
  *
  * @param params
  *   Parameters
  */
class RegisterFileMem(implicit params: Parameters) extends Module {
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

  val registers = Mem(31, UInt(64.W))

  for ((rb, i) <- io.reorderBuffer.zipWithIndex) {
    prefix(s"in$i") {
      val valid = rb.valid && rb.bits.destinationRegister =/= 0.reg
      // Check for overlapping destination registers in subsequent entries
      val overlapping =
        if (io.reorderBuffer.drop(i + 1).nonEmpty)
          io.reorderBuffer
            .drop(i + 1)
            .map(_.bits.destinationRegister === rb.bits.destinationRegister)
            .reduce(_ || _)
        else false.B
      when(valid && !overlapping) {
        registers.write(rb.bits.destinationRegister.inner - 1.U, rb.bits.value)
      }
    }
  }

  // Signals to each decoder
  for ((dec, i) <- io.decoders.zipWithIndex) {
    prefix(s"for_decoder_$i") {
      // Use 0 if the source register is 0; otherwise read from the register
      dec.values := 0.U.asTypeOf(dec.values)
      dec.values zip dec.sourceRegisters foreach { case (v, s) =>
        when(s =/= 0.reg) {
          v := registers.read(s.inner - 1.U)
        }
      }
    }
  }

  // Debug signals
  if (params.debug) {
    io.values.get(0) := 0.U
    for (i <- 1 until 32)
      io.values.get(i) := registers.read((i - 1).U)
  }
}

object RegisterFileMem extends App {
  implicit val params: b4smt.Parameters =
    Parameters(maxRegisterFileCommitCount = 2)
  ChiselStage.emitSystemVerilogFile(
    new RegisterFileMem,
    Array(),
    Array("--disable-all-randomization"),
  )
}
