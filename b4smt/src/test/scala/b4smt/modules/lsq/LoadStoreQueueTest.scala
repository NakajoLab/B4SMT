package b4smt.modules.lsq

import b4smt.Parameters
import b4smt.structures.memoryAccess.MemoryAccessInfo
import b4smt.structures.memoryAccess.MemoryAccessType._
import b4smt.structures.memoryAccess.MemoryAccessWidth._
import b4smt.utils.operations.{LoadStoreOperation, LoadStoreWidth}
import b4smt.utils.{DecodeEnqueue, LSQ2Memory, LSQfromALU, Tag}
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.{BitPat, DecoupledIO}
import chiselformal.SymbiYosysFormal
import chisel3.simulator.scalatest.ChiselSim
import chisel3.simulator.PeekPokeAPI
import org.scalatest.flatspec.AnyFlatSpec

class LoadStoreQueueWrapper(implicit params: Parameters)
    extends LoadStoreQueue
    with PeekPokeAPI {

  def initialize(): Unit = {
    this.setDecoder()
    this.setOutputs()
    this.setReorderBuffer()
  }

  def setOutputs(values: Option[LSQfromALU] = None): Unit = {
    val output = this.io.outputCollector.outputs
    val value = values
    output(0).valid.poke(value.exists(_.valid))
    output(0).bits.value.poke(value.map(_.value).getOrElse(0))
    output(0).bits.tag
      .poke(Tag(0, value.map(_.destinationtag).getOrElse(0)))
    //      output.programCounter.poke(value.map(_.ProgramCounter).getOrElse(0))

  }

  def setDecoder(
    values: Seq[Option[DecodeEnqueue]] = Seq.fill(params.decoderPerThread)(None),
  ): Unit = {
    for (i <- 0 until params.decoderPerThread) {
      val decoder = this.io.decoders(i)
      val value = values(i)
      decoder.valid.poke(value.isDefined)
      if (value.isDefined) {
        val v = value.get
        decoder.bits.destinationTag.poke(Tag(0, v.addressTag))
        decoder.bits.storeDataTag.poke(Tag(0, v.storeDataTag))
        decoder.bits.storeData.poke(v.storeData.getOrElse(0L))
        decoder.bits.storeDataValid.poke(v.storeData.isDefined)

        decoder.bits.operation.poke(v.operation)
      }

    }
  }

  def setReorderBuffer(
    DestinationTags: Seq[Int] = Seq.fill(params.maxRegisterFileCommitCount)(0),
    valids: Seq[Boolean] = Seq.fill(params.maxRegisterFileCommitCount)(false),
  ): Unit = {
    for (i <- 0 until params.maxRegisterFileCommitCount) {
      val tag = DestinationTags(i)
      val v = valids(i)
      io.reorderBuffer(i).bits.destinationTag.poke(Tag(0, tag))
      io.reorderBuffer(i).valid.poke(v)
    }
  }

  def expectMemory(values: Option[LSQ2Memory]): Unit = {
    this.io.memory.valid.expect(values.isDefined)
    if (values.isDefined) {
      this.io.memory.bits.address.expect(values.get.address)
      this.io.memory.bits.tag.id.expect(values.get.tag)
      this.io.memory.bits.operation
        .expect(values.get.operation)
      this.io.memory.bits.data.expect(values.get.data)
    }
  }

}

class LoadStoreQueueTest
    extends AnyFlatSpec
    with ChiselSim
    with SymbiYosysFormal {

  behavior of "Load Store Queue"
  implicit val defaultParams: b4smt.Parameters = Parameters(
    tagWidth = 4,
    threads = 1,
    decoderPerThread = 2,
    maxRegisterFileCommitCount = 2,
    debug = true,
  )

  it should "Both Of Instructions Enqueue LSQ" in {
    simulate(new LoadStoreQueueWrapper) { c =>
      c.initialize()
      c.io.head.get.expect(0)
      c.io.tail.get.expect(0)
      c.io.decoders(0).ready.expect(true)
      c.io.decoders(1).ready.expect(true)
      c.setDecoder(values =
        Seq(
          Some(
            DecodeEnqueue(
              addressTag = 10,
              storeDataTag = 5,
              storeData = None,
              operation = LoadStoreOperation.Load,
              operationWidth = LoadStoreWidth.Byte,
            ),
          ),
          Some(
            DecodeEnqueue(
              addressTag = 11,
              storeDataTag = 6,
              storeData = None,
              operation = LoadStoreOperation.Load,
              operationWidth = LoadStoreWidth.Byte,
            ),
          ),
        ),
      )
      c.clock.step(1)

      c.setDecoder()
      c.io.head.get.expect(2)
      c.io.tail.get.expect(0)
      c.clock.step(2)
    }
  }

  it should "check formal" in {
    symbiYosysCheck(
      new LoadStoreQueueWrapper()(
        defaultParams.copy(loadStoreQueueIndexWidth = 2),
      ),
      depth = 10,
    )
  }

// TODO: Restore this
//  it should "load check" in {
//    // runParallel = 1, maxRegisterFileCommitCount = 1
//    simulate(new LoadStoreQueueWrapper) {
//      c =>
//        c.initialize()
//        // Initialization
//        c.io.head.get.expect(0)
//        c.io.tail.get.expect(0)
//        c.io.decoders(0).ready.expect(true)
//        c.io.memory.ready.poke(true)
//        // c.io.memory(1).ready.poke(true) (if runParallel = 2)
//        c.expectMemory(None)
//        // c.expectMemory(Seq(None, None)) (if runParallel = 2)
//
//        // Set values
//        c.setDecoder(values =
//          Seq(
//            Some(
//              DecodeEnqueue(
//                addressTag = 10,
//                storeDataTag = 5,
//                storeData = Some(0),
//                operation = LoadStoreOperation.Load,
//                operationWidth = LoadStoreWidth.Byte
//              )
//            ),
//            None
//          )
//        )
//        c.clock.step(1)
//
//        c.setDecoder()
//        c.setOutputs(values =
//          Some(LSQfromALU(valid = true, destinationtag = 10, value = 150))
//        )
//        c.io.head.get.expect(1)
//        c.io.tail.get.expect(0)
//        c.expectMemory(None)
//        c.clock.step(1)
//
//        // Check values
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 150,
//              tag = 10,
//              data = 0,
//              operation = LoadStoreOperation.Load,
//              operationWidth = LoadStoreWidth.Byte
//            )
//          )
//        )
//        c.io.tail.get.expect(0)
//        c.io.head.get.expect(1)
//        c.clock.step(2)
//
//        c.io.head.get.expect(1)
//        c.io.tail.get.expect(1)
//        c.clock.step(3)
//
//    }
//  }

  // TODO: Restore this
//  it should "store check" in {
//    // runParallel = 1, maxRegisterFileCommitCount = 1
//    simulate(new LoadStoreQueueWrapper) {
//      c =>
//        c.initialize()
//        // Initialization
//        c.io.head.get.expect(0)
//        c.io.tail.get.expect(0)
//        c.io.decoders(0).ready.expect(true)
//        c.io.memory.ready.poke(true)
//        c.io.memory.ready.poke(true)
//        // c.io.memory(1).ready.poke(true) (if runParallel = 2)
//        c.expectMemory(None)
//        // c.expectMemory(Seq(None, None)) (if runParallel = 2)
//
//        // Set values
//        c.setDecoder(values =
//          Seq(
//            Some(
//              DecodeEnqueue(
//                addressTag = 10,
//                storeDataTag = 5,
//                storeData = None,
//                operation = LoadStoreOperation.Load,
//                operationWidth = LoadStoreWidth.Byte
//              )
//            ),
//            None
//          )
//        )
//        c.clock.step(1)
//
//        c.setDecoder()
//        c.setOutputs(values =
//          Some(LSQfromALU(valid = true, destinationtag = 5, value = 123))
//        )
//        c.io.head.get.expect(1)
//        c.io.tail.get.expect(0)
//        c.clock.step(1)
//
//        c.setOutputs(values =
//          Some(LSQfromALU(valid = true, destinationtag = 10, value = 150))
//        )
//        c.setReorderBuffer(
//          valids = Seq(true, false),
//          DestinationTags = Seq(10, 1)
//        )
//        c.io.head.get.expect(1)
//        c.io.tail.get.expect(0)
//        c.expectMemory(None)
//        c.clock.step()
//
//        // Check values
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 150,
//              tag = 10,
//              data = 123,
//              operation = LoadStoreOperation.Load,
//              operationWidth = LoadStoreWidth.Byte
//            )
//          )
//        )
//        c.io.head.get.expect(1)
//        c.io.tail.get.expect(0)
//        c.clock.step(2)
//
//        c.io.head.get.expect(1)
//        c.io.tail.get.expect(1)
//        c.clock.step(1)
//    }
//  }

  // TODO: Restore this
//  it should "2 Parallel load check" in {
//    simulate(new LoadStoreQueueWrapper) {
//      c =>
//        c.initialize()
//        // Initialization
//        c.io.head.get.expect(0)
//        c.io.tail.get.expect(0)
//        c.io.decoders(0).ready.expect(true)
//        c.io.decoders(1).ready.expect(true)
//        c.io.memory.ready.poke(true)
//        c.io.memory.ready.poke(true)
//        c.expectMemory(None)
//
//        // Set values
//        c.setDecoder(values =
//          Seq(
//            Some(
//              DecodeEnqueue(
//                addressTag = 10,
//                storeDataTag = 5,
//                storeData = None,
//                operation = LoadStoreOperation.Load,
//                operationWidth = LoadStoreWidth.Byte
//              )
//            ),
//            Some(
//              DecodeEnqueue(
//                addressTag = 11,
//                storeDataTag = 6,
//                storeData = None,
//                operation = LoadStoreOperation.Load,
//                operationWidth = LoadStoreWidth.Byte
//              )
//            )
//          )
//        )
//        c.clock.step(1)
//
//        c.setDecoder()
//        c.io.head.get.expect(2)
//        c.io.tail.get.expect(0)
//        c.setOutputs(values =
//          Some(LSQfromALU(valid = true, destinationtag = 10, value = 150))
//        )
//        c.clock.step()
//
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 150,
//              tag = 10,
//              data = 0,
//              operation = LoadStoreOperation.Load,
//              operationWidth = LoadStoreWidth.Byte
//            )
//          )
//        )
//
//        c.setOutputs(
//          Some(LSQfromALU(valid = true, destinationtag = 11, value = 100))
//        )
//        c.clock.step()
//
//        // Check values
//        c.setOutputs()
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 100,
//              tag = 11,
//              data = 0,
//              operation = LoadStoreOperation.Load,
//              operationWidth = LoadStoreWidth.Byte
//            )
//          )
//        )
//        c.clock.step(2)
//
//    }
//  }

//  it should "2 Parallel store check" in {
//    simulate(new LoadStoreQueueWrapper) {
//      c =>
//        c.initialize()
//        // Initialization
//        c.io.head.get.expect(0)
//        c.io.tail.get.expect(0)
//        c.io.decoders(0).ready.expect(true)
//        c.io.decoders(1).ready.expect(true)
//        c.io.memory.ready.poke(true)
//        c.io.memory.ready.poke(true)
//        c.expectMemory(None)
//
//        // Set values
//        c.setDecoder(values =
//          Seq(
//            Some(
//              DecodeEnqueue(
//                addressTag = 10,
//                storeDataTag = 5,
//                storeData = None,
//                operation = LoadStoreOperation.Load8
//              )
//            ),
//            Some(
//              DecodeEnqueue(
//                addressTag = 11,
//                storeDataTag = 6,
//                storeData = Some(123),
//                operation = LoadStoreOperation.Store8
//              )
//            )
//          )
//        )
//        c.clock.step(1)
//
//        c.setDecoder()
//        c.io.head.get.expect(2)
//        c.io.tail.get.expect(0)
//        c.setOutputs(values =
//          Some(
//            LSQfromALU(
//              valid = true,
//              destinationtag = 10,
//              value = 150,
//              resultType = ResultType.LoadStoreAddress
//            )
//          )
//        )
//        c.clock.step(1)
//        c.setOutputs(values =
//          Some(LSQfromALU(valid = true, destinationtag = 5, value = 100))
//        )
//        c.clock.step(1)
//
//        c.setOutputs(values =
//          Some(
//            LSQfromALU(
//              valid = true,
//              destinationtag = 11,
//              value = 456,
//              resultType = ResultType.LoadStoreAddress
//            )
//          )
//        )
//        c.setReorderBuffer(
//          valids = Seq(false, true),
//          DestinationTags = Seq(1, 10)
//        )
//        c.clock.step(1)
//
//        // Check values
//        c.setOutputs()
//        c.setReorderBuffer(
//          valids = Seq(true, false),
//          DestinationTags = Seq(11, 15)
//        )
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 150,
//              tag = 10,
//              data = 100,
//              operation = LoadStoreOperation.Load8
//            )
//          )
//        )
//        c.clock.step(1)
//
//        c.setReorderBuffer()
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 456,
//              tag = 11,
//              data = 123,
//              operation = LoadStoreOperation.Load8
//            )
//          )
//        )
//        c.clock.step(2)
//
//    }
//  }
//
//  it should "2 Parallel check (1 clock wait)" in {
//    simulate(new LoadStoreQueueWrapper) {
//      c =>
//        c.initialize()
//        // Initialization
//        c.io.head.get.expect(0)
//        c.io.tail.get.expect(0)
//        c.io.decoders(0).ready.expect(true)
//        c.io.decoders(1).ready.expect(true)
//        c.io.memory.ready.poke(true)
//        c.io.memory.ready.poke(true)
//        c.expectMemory(None)
//
//        // Set values
//        c.setDecoder(values =
//          Seq(
//            Some(
//              DecodeEnqueue(
//                addressTag = 10,
//                storeDataTag = 5,
//                storeData = None,
//                operation = LoadStoreOperation.Load8
//              )
//            ),
//            Some(
//              DecodeEnqueue(
//                addressTag = 11,
//                storeDataTag = 6,
//                storeData = None,
//                operation = LoadStoreOperation.Load8
//              )
//            )
//          )
//        )
//        // load instruction + store instruction
//        c.clock.step(1)
//
//        c.setDecoder(values =
//          Seq(
//            None,
//            Some(
//              DecodeEnqueue(
//                addressTag = 13,
//                storeDataTag = 0,
//                storeData = None,
//                operation = LoadStoreOperation.Load8
//              )
//            )
//          )
//        )
//        // invalid instruction + load instruction
//        c.io.head.get.expect(2)
//        c.io.tail.get.expect(0)
//        c.setOutputs(
//          Some(
//            LSQfromALU(
//              valid = true,
//              destinationtag = 10,
//              value = 150,
//              resultType = ResultType.LoadStoreAddress
//            )
//          ) // 1st load address
//        )
//        c.clock.step(1)
//        c.setDecoder()
//
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 150,
//              tag = 10,
//              data = 0,
//              operation = LoadStoreOperation.Load8
//            )
//          )
//        )
//
//        c.setOutputs(
//          Some(
//            LSQfromALU(
//              valid = true,
//              destinationtag = 11,
//              value = 100,
//              resultType = ResultType.LoadStoreAddress
//            )
//          ) // store address
//        )
//        c.clock.step()
//
//        // Check values
//        c.setDecoder()
//
//        c.io.head.get.expect(3)
//        c.io.tail.get.expect(0)
//        c.setOutputs(values =
//          Some(
//            LSQfromALU(
//              valid = true,
//              destinationtag = 13,
//              value = 123,
//              resultType = ResultType.LoadStoreAddress
//            )
//          )
//        ) // 2nd load address
//        c.clock.step(1)
//
//        c.setOutputs(values =
//          Some(
//            LSQfromALU(valid = true, destinationtag = 6, value = 456)
//          ) // store data
//        )
//        // Issue a load instruction ahead of a store instruction
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 123,
//              tag = 13,
//              data = 0,
//              operation = LoadStoreOperation.Load8
//            )
//          )
//        )
//        c.clock.step(1)
//
//        c.setReorderBuffer(
//          valids = Seq(false, true),
//          DestinationTags = Seq(1, 11)
//        )
//        c.setOutputs()
//        c.expectMemory(None)
//        c.clock.step(1)
//
//        c.expectMemory(values =
//          Some(
//            LSQ2Memory(
//              address = 100,
//              tag = 11,
//              data = 456,
//              operation = LoadStoreOperation.Load8
//            )
//          )
//        )
//        c.clock.step(2)
//    }
//  }
//
//  // Check that loads do not overtake stores
//  // Check for the bug that set Overlap=false when the store address was not yet determined
//  it should "wait load for the overlapping store" in {
//    // runParallel = 1, maxRegisterFileCommitCount = 1
//    simulate(
//      new LoadStoreQueueWrapper()(
//        defaultParams.copy(decoderPerThread = 1, maxRegisterFileCommitCount = 1)
//      )
//    ) { c =>
//      c.initialize()
//      // Initialization
//      c.io.head.get.expect(0)
//      c.io.tail.get.expect(0)
//      c.io.decoders(0).ready.expect(true)
//      c.io.memory.ready.poke(true)
//      // c.io.memory(1).ready.poke(true) (if runParallel = 2)
//      c.expectMemory(None)
//      // c.expectMemory(Seq(None, None)) (if runParallel = 2)
//
//      // Add a store instruction
//      c.setDecoder(values =
//        Seq(
//          Some(
//            DecodeEnqueue(
//              addressTag = 10,
//              storeDataTag = 5,
//              storeData = None,
//              operation = LoadStoreOperation.Load8
//            )
//          )
//        )
//      )
//
//      c.clock.step()
//      // Add a load instruction
//      c.setDecoder(values =
//        Seq(
//          Some(
//            DecodeEnqueue(
//              addressTag = 11,
//              storeDataTag = 0,
//              storeData = None,
//              operation = LoadStoreOperation.Load8
//            )
//          )
//        )
//      )
//      c.io.head.get.expect(1)
//      c.io.tail.get.expect(0)
//
//      c.clock.step()
//      c.initialize()
//      c.io.head.get.expect(2)
//      c.io.tail.get.expect(0)
//
//      c.clock.step(2)
//      // Specify the load instruction's address first
//      c.setOutputs(values =
//        Some(
//          LSQfromALU(
//            valid = true,
//            destinationtag = 11,
//            value = 150,
//            resultType = ResultType.LoadStoreAddress
//          )
//        )
//      )
//      c.expectMemory(None)
//
//      c.clock.step()
//      c.initialize()
//      // No output yet
//      c.expectMemory(None)
//
//      c.clock.step()
//      // Determine the store instruction's values
//      c.setOutputs(values =
//        Some(
//          LSQfromALU(
//            valid = true,
//            destinationtag = 10,
//            value = 150,
//            resultType = ResultType.LoadStoreAddress
//          )
//        )
//      )
//      c.clock.step()
//      c.setOutputs(values =
//        Some(LSQfromALU(valid = true, destinationtag = 5, value = 300))
//      )
//      c.setReorderBuffer(Seq(10), Seq(true))
//
//      c.clock.step()
//      c.initialize()
//      // Issue the store instruction
//      c.expectMemory(values =
//        Some(
//          LSQ2Memory(
//            address = 150,
//            tag = 10,
//            data = 300,
//            operation = LoadStoreOperation.Load8
//          )
//        )
//      )
//
//      c.clock.step()
//      // Issue the load instruction
//      c.expectMemory(values =
//        Some(
//          LSQ2Memory(
//            address = 150,
//            tag = 11,
//            data = 0,
//            operation = LoadStoreOperation.Load8
//          )
//        )
//      )
//
//      c.clock.step(2)
//
//      c.io.tail.get.expect(2)
//      c.io.head.get.expect(2)
//
//      c.clock.step(3)
//    }
//  }
//
}
