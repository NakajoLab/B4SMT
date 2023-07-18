package b4processor.modules.reorderbuffer

import b4processor.Parameters
import b4processor.utils.RVRegister.{AddRegConstructor, AddUIntRegConstructor}
import b4processor.utils.{DecoderValue, ExecutorValue, RegisterFileValue, Tag}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._

import scala.util.Random

class ReorderBufferWrapper(implicit params: Parameters)
    extends ReorderBuffer() {
  def initialize(): Unit = {
    setOutputs()
    setDecoder()
  }

  def setOutputs(values: Option[ExecutorValue] = None): Unit = {
    val output = this.io.collectedOutputs.outputs
    val v = values
    output(0).valid.poke(v.isDefined)
    if (v.isDefined) {
      output(0).bits.tag.poke(Tag(0, v.get.destinationTag))
      output(0).bits.value.poke(v.get.value)
    }
  }

  def setDecoder(
    decoderValues: Seq[DecoderValue] =
      Seq.fill(params.decoderPerThread)(DecoderValue())
  ): Unit = {
    for (i <- 0 until params.decoderPerThread) {
      val decoder = this.io.decoders(i)
      val values = decoderValues(i)
      decoder.valid.poke(values.valid)
      decoder.source1.sourceRegister.poke(values.source1)
      decoder.source2.sourceRegister.poke(values.source2)
      decoder.destination.destinationRegister.poke(values.destination)
    }
  }

  def expectRegisterFile(outputs: Seq[Option[RegisterFileValue]]): Unit = {
    for (i <- 0 until params.maxRegisterFileCommitCount) {
      this.io.registerFile(i).valid.expect(outputs(i).isDefined)
      if (outputs(i).isDefined) {
        this.io
          .registerFile(i)
          .bits
          .destinationRegister
          .expect(outputs(i).get.destinationRegister.reg)
        this.io.registerFile(i).bits.value.expect(outputs(i).get.value)
      }
    }
  }

  def printRF(): Unit = {
    for (i <- 0 until params.maxRegisterFileCommitCount)
      println(s"rf valid=${this.io.registerFile(i).valid.peek()} rd=${this.io
          .registerFile(i)
          .bits
          .destinationRegister
          .peek()}, value=${this.io.registerFile(i).bits.value.peek()}")
  }
}

class ReorderBufferTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Reorder Buffer"
  implicit val defaultParams = Parameters(
    tagWidth = 4,
    decoderPerThread = 1,
    maxRegisterFileCommitCount = 1,
    debug = true
  )

  /** リオーダバッファに値が出力されない */
  it should "output nothing to register file on first clock" in {
    test(new ReorderBufferWrapper) { c =>
      c.initialize()
      c.expectRegisterFile(Seq(None))
    }
  }

  it should "set ready to 0 when full" in {
    test(new ReorderBufferWrapper) { c =>
      c.initialize()
      var loop = 0
      c.io.decoders(0).ready.expect(true)
      while (loop < 40 && c.io.decoders(0).ready.peek().litToBoolean) {
        //        println(loop, c.io.head.get.peek().litValue, c.io.tail.get.peek().litValue)
        c.setDecoder(
          Seq(
            DecoderValue(
              valid = true,
              source1 = Random.nextInt(32).reg,
              source2 = Random.nextInt(32).reg,
              destination = Random.nextInt(32).reg
            )
          )
        )
        c.clock.step()
        loop += 1
      }
      c.io.decoders(0).ready.expect(false)
    }
  }

  it should "set ready to 0 when full with 4 decoders (reduced tag width to 5 for speed)" in {
    test(new ReorderBufferWrapper()(defaultParams.copy(decoderPerThread = 4))) {
      c =>
        c.initialize()
        var loop = 0
        c.io.decoders(0).ready.expect(true)
        while (loop < 40 && c.io.decoders(0).ready.peek().litToBoolean) {
          //        println(loop, c.io.head.get.peek().litValue, c.io.tail.get.peek().litValue)
          c.setDecoder(
            Seq(
              DecoderValue(
                valid = true,
                source1 = Random.nextInt(32).reg,
                source2 = Random.nextInt(32).reg,
                destination = Random.nextInt(32).reg
              ),
              DecoderValue(
                valid = true,
                source1 = Random.nextInt(32).reg,
                source2 = Random.nextInt(32).reg,
                destination = Random.nextInt(32).reg
              ),
              DecoderValue(
                valid = true,
                source1 = Random.nextInt(32).reg,
                source2 = Random.nextInt(32).reg,
                destination = Random.nextInt(32).reg
              ),
              DecoderValue(
                valid = true,
                source1 = Random.nextInt(32).reg,
                source2 = Random.nextInt(32).reg,
                destination = Random.nextInt(32).reg
              )
            )
          )
          c.clock.step()
          loop += 1
        }
        //      println(loop, c.io.head.get.peek().litValue, c.io.tail.get.peek().litValue)
        c.io.decoders(0).ready.expect(false)
    }
  }

  it should "set ready to 0 when full with 4 decoders but 2 decoder inputs (reduced tag width to 5 for speed)" in {
    test(new ReorderBufferWrapper()(defaultParams.copy(decoderPerThread = 4))) {
      c =>
        c.initialize()
        var loop = 0
        c.io.decoders(0).ready.expect(true)
        while (loop < 40 && c.io.decoders(0).ready.peek().litToBoolean) {
          //        println(loop, c.io.head.get.peek().litValue, c.io.tail.get.peek().litValue)
          c.setDecoder(
            Seq(
              DecoderValue(
                valid = true,
                source1 = Random.nextInt(32).reg,
                source2 = Random.nextInt(32).reg,
                destination = Random.nextInt(32).reg
              ),
              DecoderValue(
                valid = true,
                source1 = Random.nextInt(32).reg,
                source2 = Random.nextInt(32).reg,
                destination = Random.nextInt(32).reg
              ),
              DecoderValue(),
              DecoderValue()
            )
          )
          c.clock.step()
          loop += 1
        }
        //      println(loop, c.io.head.get.peek().litValue, c.io.tail.get.peek().litValue)
        c.io.decoders(0).ready.expect(false)
    }
  }

  it should "make the tail follow head" in {
    test(new ReorderBufferWrapper) { c =>
      c.initialize()
      c.io.decoders(0).ready.expect(true)
      c.io.tail.get.expect(0)
      //      println(c.io.head.get.peek().litValue, c.io.tail.get.peek().litValue)
      c.setDecoder(
        Seq(
          DecoderValue(
            valid = true,
            destination = 1.reg,
            source1 = 2.reg,
            source2 = 3.reg
          )
        )
      )
      c.setOutputs(Some(ExecutorValue(destinationTag = 0, value = 3)))
      c.clock.step(2)

      //      println(c.io.head.get.peek().litValue, c.io.tail.get.peek().litValue)
      c.io.tail.get.expect(1)
    }
  }

  it should "have an output in register file" in {
    test(new ReorderBufferWrapper).withAnnotations(Seq(WriteVcdAnnotation)) {
      c =>
        c.initialize()
        c.clock.setTimeout(10)

        // 値の確認
        c.io.head.get.expect(0)
        c.io.tail.get.expect(0)
        c.expectRegisterFile(Seq(None))

        // 値のセット
        c.setDecoder(
          Seq(
            DecoderValue(
              valid = true,
              destination = 1.reg,
              source1 = 2.reg,
              source2 = 3.reg,
              programCounter = 500
            )
          )
        )

        c.clock.step()
        // 値の確認
        c.expectRegisterFile(Seq(None))
        c.io.head.get.expect(1)

        // 値のセット
        c.setDecoder(Seq(DecoderValue()))
        c.setOutputs(Some(ExecutorValue(destinationTag = 0, value = 10)))

        c.clock.step()
        c.setOutputs(None)
        // 値の確認
        c.expectRegisterFile(
          Seq(Some(RegisterFileValue(destinationRegister = 1, value = 10)))
        )

        c.clock.step(5)
    }
  }

  it should "have an output in register file with 4 of each component" in {
    test(
      new ReorderBufferWrapper()(
        defaultParams.copy(decoderPerThread = 4, maxRegisterFileCommitCount = 4)
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      c.clock.setTimeout(10)

      // 値の確認
      c.expectRegisterFile(Seq(None, None, None, None))

      // 値のセット
      c.setDecoder(
        Seq(
          DecoderValue(
            valid = true,
            destination = 1.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 500
          ),
          DecoderValue(
            valid = true,
            destination = 2.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 500
          ),
          DecoderValue(
            valid = true,
            destination = 3.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 500
          ),
          DecoderValue(
            valid = true,
            destination = 4.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 500
          )
        )
      )

      c.clock.step()
      // 値の確認
      c.expectRegisterFile(Seq(None, None, None, None))

      // 値のセット
      c.setDecoder(Seq.fill(4)(DecoderValue()))

      c.setOutputs(Some(ExecutorValue(destinationTag = 0, value = 10)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 1, value = 10)),
          None,
          None,
          None
        )
      )
      c.setOutputs(Some(ExecutorValue(destinationTag = 1, value = 20)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 2, value = 20)),
          None,
          None,
          None
        )
      )
      c.setOutputs(Some(ExecutorValue(destinationTag = 2, value = 30)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 3, value = 30)),
          None,
          None,
          None
        )
      )
      c.setOutputs(Some(ExecutorValue(destinationTag = 3, value = 40)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 4, value = 40)),
          None,
          None,
          None
        )
      )

      c.setOutputs(None)
      c.io.registerFile(0).valid.expect(true)

      c.clock.step(5)
    }
  }

  it should "have an output in register file with 4 of each component out of order simple" in {
    test(
      new ReorderBufferWrapper()(
        defaultParams.copy(decoderPerThread = 4, maxRegisterFileCommitCount = 4)
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      c.clock.setTimeout(10)

      // 値の確認
      c.expectRegisterFile(Seq(None, None, None, None))

      // 値のセット
      c.setDecoder(
        Seq(
          DecoderValue(
            valid = true,
            destination = 1.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 500
          ),
          DecoderValue(
            valid = true,
            destination = 2.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 504
          ),
          DecoderValue(
            valid = true,
            destination = 3.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 508
          ),
          DecoderValue(
            valid = true,
            destination = 4.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 512
          )
        )
      )

      c.clock.step()

      // 値のセット
      c.setDecoder(
        Seq(
          DecoderValue(
            valid = true,
            destination = 5.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 516
          ),
          DecoderValue(
            valid = true,
            destination = 6.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 520
          ),
          DecoderValue(
            valid = true,
            destination = 7.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 524
          ),
          DecoderValue(
            valid = true,
            destination = 8.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 528
          )
        )
      )
      c.clock.step()
      c.setDecoder()

      c.setOutputs(Some(ExecutorValue(destinationTag = 4, value = 50)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 5, value = 60)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 6, value = 70)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 7, value = 80)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 0, value = 10)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 1, value = 10)),
          None,
          None,
          None
        )
      )
      c.setOutputs(Some(ExecutorValue(destinationTag = 1, value = 20)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 2, value = 20)),
          None,
          None,
          None
        )
      )
      c.setOutputs(Some(ExecutorValue(destinationTag = 2, value = 30)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 3, value = 30)),
          None,
          None,
          None
        )
      )
      c.setOutputs(Some(ExecutorValue(destinationTag = 3, value = 40)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 4, value = 40)),
          Some(RegisterFileValue(destinationRegister = 5, value = 50)),
          Some(RegisterFileValue(destinationRegister = 6, value = 60)),
          Some(RegisterFileValue(destinationRegister = 7, value = 70))
        )
      )

      c.clock.step()
      c.setOutputs(None)
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 8, value = 80)),
          None,
          None,
          None
        )
      )

      c.clock.step(5)
    }
  }

  it should "have an output in register file with 4 of each component out of order complex" in {
    test(
      new ReorderBufferWrapper()(
        defaultParams.copy(decoderPerThread = 4, maxRegisterFileCommitCount = 4)
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.initialize()
      c.clock.setTimeout(10)

      // 値の確認
      c.expectRegisterFile(Seq(None, None, None, None))

      // 値のセット
      c.setDecoder(
        Seq(
          DecoderValue(
            valid = true,
            destination = 1.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 500
          ),
          DecoderValue(
            valid = true,
            destination = 2.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 504
          ),
          DecoderValue(
            valid = true,
            destination = 3.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 508
          ),
          DecoderValue(
            valid = true,
            destination = 4.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 512
          )
        )
      )

      c.clock.step()

      // 値のセット
      c.setDecoder(
        Seq(
          DecoderValue(
            valid = true,
            destination = 5.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 516
          ),
          DecoderValue(
            valid = true,
            destination = 6.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 520
          ),
          DecoderValue(
            valid = true,
            destination = 7.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 524
          ),
          DecoderValue(
            valid = true,
            destination = 8.reg,
            source1 = 2.reg,
            source2 = 3.reg,
            programCounter = 528
          )
        )
      )
      c.clock.step()
      c.setDecoder()

      c.setOutputs(Some(ExecutorValue(destinationTag = 1, value = 20)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 5, value = 60)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 7, value = 80)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 2, value = 30)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 0, value = 10)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 1, value = 10)),
          Some(RegisterFileValue(destinationRegister = 2, value = 20)),
          Some(RegisterFileValue(destinationRegister = 3, value = 30)),
          None
        )
      )
      c.setOutputs(Some(ExecutorValue(destinationTag = 6, value = 70)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 4, value = 50)))
      c.clock.step()
      c.setOutputs(Some(ExecutorValue(destinationTag = 3, value = 40)))
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 4, value = 40)),
          Some(RegisterFileValue(destinationRegister = 5, value = 50)),
          Some(RegisterFileValue(destinationRegister = 6, value = 60)),
          Some(RegisterFileValue(destinationRegister = 7, value = 70))
        )
      )
      c.clock.step()
      c.expectRegisterFile(
        Seq(
          Some(RegisterFileValue(destinationRegister = 8, value = 80)),
          None,
          None,
          None
        )
      )

      c.setOutputs(None)

      c.clock.step(5)
    }
  }
}
