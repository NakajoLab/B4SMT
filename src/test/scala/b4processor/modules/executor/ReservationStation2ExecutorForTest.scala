package b4processor.modules.executor

import b4processor.Parameters
import b4processor.utils.Tag
import b4processor.utils.operations.ALUOperation
import chisel3.util.ReadyValidIO
import chisel3._

class ReservationStation2ExecutorForTest(implicit params: Parameters)
    extends ReadyValidIO(new Bundle {
      val destinationTag = new Tag
      val value1 = SInt(64.W)
      val value2 = SInt(64.W)
      val operation = ALUOperation()
      val programCounter = UInt(64.W)
      val wasCompressed = Bool()
      val branchOffset = SInt(12.W)
    })
