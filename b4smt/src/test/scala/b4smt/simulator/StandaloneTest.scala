package b4smt.simulator

import b4smt.Parameters
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import org.scalatest.{Outcome, TestSuite, TestSuiteMixin}

/** One build per complete immutable Parameters value in this JVM; every plan
  * gets a fresh native process. No Mill/Nix subprocess, svsim, or cross-JVM
  * cache.
  */
object StandaloneTest {
  private val binaries = mutable.Map.empty[Parameters, Path]
  private lazy val root = {
    val parent = Paths.get("test_run_dir", "standalone").toAbsolutePath
    Files.createDirectories(parent)
    Files.createTempDirectory(parent, "jvm-")
  }

  private def run(command: Seq[String], cwd: Path, log: Path): Unit = {
    val process = new ProcessBuilder(command: _*)
      .directory(cwd.toFile)
      .redirectErrorStream(true)
      .redirectOutput(log.toFile)
      .start()
    val status = process.waitFor()
    assert(
      status == 0,
      s"${command.mkString(" ")} exited $status; diagnostics: $log\n${Files.readString(log)}",
    )
  }

  private def binary(params: Parameters): Path = synchronized {
    binaries.getOrElseUpdate(
      params, {
        val dir = Files.createTempDirectory(root, "build-")
        val config = dir.resolve("config.json")
        Files.writeString(
          config,
          ujson.write(StandaloneSimulator.config(params), indent = 2),
        )
        sys.env.get("B4SMT_SIMULATOR_DIR") match {
          case Some(prebuilt) =>
            val path = Paths.get(prebuilt).toAbsolutePath
            require(
              StandaloneSimulator.readConfig(
                path.resolve("share/b4smt-simulator/config.json").toString,
              ) == params,
              s"prebuilt simulator configuration differs from $config",
            )
            path.resolve("bin/b4smt-sim")
          case None =>
            val start = System.nanoTime()
            println(s"STANDALONE BUILD $dir config=$config")
            // Use the same JSON generator entry point and compiler as the Nix package.
            StandaloneSimulator.main(
              Array("--config", config.toString, "--target-dir", dir.toString),
            )
            run(
              Seq(
                "verilator",
                "--cc",
                "--exe",
                "--build",
                "--assert",
                "-Wno-fatal",
                "--top-module",
                "StandaloneSimulator",
                "-j",
                sys.env.getOrElse("B4SMT_SIM_JOBS", "2"),
                "-f",
                "filelist.f",
                Paths.get("simulator", "main.cpp").toAbsolutePath.toString,
                "-o",
                "b4smt-sim",
              ),
              dir,
              dir.resolve("compile.log"),
            )
            println(
              f"STANDALONE BUILT $dir in ${(System.nanoTime() - start) / 1e9}%.3f s",
            )
            dir.resolve("obj_dir/b4smt-sim")
        }
      },
    )
  }

  def execute(params: Parameters, name: String)(body: Plan => Unit): Unit = {
    val plan = new Plan
    body(plan)
    require(plan.program.nonEmpty, "plan must initialize a program")
    val executable = binary(params)
    val dir = Files.createTempDirectory(root, "session-")
    Files.writeString(dir.resolve("test-name.txt"), name)
    val script = dir.resolve("plan.txt")
    Files.writeString(script, plan.actions.mkString("", "\n", "\n"))
    println(s"STANDALONE SESSION $dir binary=$executable test=$name")
    run(
      Seq(
        executable.toString,
        "--program",
        plan.program,
        "--format",
        plan.format,
        "--plan",
        script.toString,
      ),
      Paths.get("").toAbsolutePath,
      dir.resolve("run.log"),
    )
    print(Files.readString(dir.resolve("run.log")))
  }

  final class Plan {
    private[StandaloneTest] var program = ""
    private[StandaloneTest] var format = "bytes"
    private[StandaloneTest] val actions = mutable.ArrayBuffer.empty[String]
    private def add(op: String, args: Any*): Unit =
      actions += (op + " " + args.mkString(" "))
    def initialize(path: String, binary: Boolean = false): Unit = {
      require(program.isEmpty, "only one program per plan")
      program =
        Paths.get(if (binary) path else path + ".hex").toAbsolutePath.toString
      format = if (binary) "binary" else "bytes"
    }
    def initialize64(path: String): Unit = {
      require(program.isEmpty, "only one program per plan")
      program = Paths.get(path + ".64.hex").toAbsolutePath.toString
      format = "words"
    }
    def step(cycles: Int = 1): Unit = add("step", cycles)
    def expectRegister(reg: Int, value: BigInt, thread: Int = 0): Unit =
      add("expect", thread, reg, value)
    def checkForRegister(
      reg: Int,
      value: BigInt,
      timeout: Int = 500,
      thread: Int = 0,
    ): Unit =
      add("wait", thread, reg, value, timeout)
    def checkForRegisterChange(
      reg: Int,
      value: BigInt,
      timeout: Int = 500,
      thread: Int = 0,
    ): Unit =
      add("change", thread, reg, value, timeout)
    def outputReady(ready: Boolean): Unit = add("ready", if (ready) 1 else 0)
    def checkForOutput(
      value: Char,
      timeout: Int = 500,
      print_value: Boolean = false,
    ): Unit =
      add("uart", timeout, if (print_value) 1 else 0, value.toInt)
    def checkForOutputAny(
      timeout: Int = 500,
      print_value: Boolean = false,
    ): Unit =
      add("any", timeout, if (print_value) 1 else 0)
    private def search(
      op: String,
      text: String,
      timeout: Int,
      print: Boolean,
      maxCharacters: Long,
    ): Unit = {
      require(text.nonEmpty)
      add(
        op,
        timeout,
        if (print) 1 else 0,
        maxCharacters,
        text.length,
        text.map(_.toInt).mkString(" "),
      )
    }
    def checkForOutputString(
      str: String,
      char_timeout: Int = 500,
      print_value: Boolean = false,
    ): Unit =
      search(
        "search",
        str,
        char_timeout,
        print_value,
        math.max(1L, str.length.toLong) * char_timeout,
      )
    // Sliding suffix matches benchmark's rolling three-byte window, unlike the
    // fixture's reset-on-mismatch substring search above.
    def outputUntilSuffix(
      text: String,
      timeout: Int,
      maxCharacters: Long,
      print: Boolean,
    ): Unit =
      search("suffix", text, timeout, print, maxCharacters)
    def dumpRegisters(): Unit = add("dump")
    def monitorWrites(cycles: Int, address: Long): Unit =
      add("monitor", cycles, address)
    def statistics(path: String, fields: ujson.Obj): Unit = {
      val prefix = ujson.write(fields).dropRight(1) + ","
      add(
        "stats",
        ujson.write(Paths.get(path).toAbsolutePath.toString),
        ujson.write(prefix),
      )
    }
  }
}

trait StandaloneTests extends TestSuiteMixin { this: TestSuite =>
  private var standaloneTestName = ""
  abstract override def withFixture(test: NoArgTest): Outcome = {
    standaloneTestName = test.name
    super.withFixture(test)
  }
  def standalone(params: Parameters)(body: StandaloneTest.Plan => Unit): Unit =
    StandaloneTest.execute(params, standaloneTestName)(body)
}
