package b4smt.simulator

import b4smt.Parameters
import java.nio.file.Files
import scala.util.control.NonFatal

// Small parser checks, independent of RTL elaboration and ScalaTest workflows.
object StandaloneConfigTest {
  def main(args: Array[String]): Unit = {
    val file = Files.createTempFile("b4smt-config", ".json")
    def parse(json: String): Parameters = {
      Files.writeString(file, json)
      StandaloneSimulator.readConfig(file.toString)
    }
    try {
      assert(parse("""{"debug":true}""") == Parameters(debug = true))
      val complete = Parameters(
        debug = true,
        threads = 5,
        decoderPerThread = 4,
        executors = 8,
        tagWidth = 6,
        enablePExt = true,
        instructionStart = 0x80000000L,
      )
      val serialized = StandaloneSimulator.config(complete)
      assert(serialized.obj.keySet.toSet == complete.productElementNames.toSet)
      assert(parse(ujson.write(serialized)) == complete)
      assert(
        parse(
          """{"debug":true,"instructionStart":"0x80100000"}""",
        ).instructionStart == 0x80100000L,
      )
      assert(
        parse(
          """{"debug":true,"instructionStart":2148532224}""",
        ).instructionStart == 0x80100000L,
      )
      val invalid = Seq(
        "{",
        "[]",
        "{}",
        """{"debug":true,"typo":1}""",
        """{"debug":1}""",
        """{"debug":true,"threads":1.5}""",
        """{"debug":true,"threads":0}""",
        """{"debug":true,"threads":2147483648}""",
        """{"debug":true,"threads":"2"}""",
        """{"debug":true,"instructionStart":1}""",
        """{"debug":true,"instructionStart":9007199254740992}""",
      )
      for (json <- invalid) {
        val rejected =
          try {
            parse(json)
            false
          } catch { case NonFatal(_) => true }
        assert(rejected, s"configuration should have been rejected: $json")
      }
      println(
        s"PASS configuration defaults, address formats, ${invalid.size} invalid configurations",
      )
    } finally Files.delete(file)
  }
}
