package b4smt.simulator

import java.nio.file.Files
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import org.scalatest.funsuite.AnyFunSuite

class StandaloneProcessTest extends AnyFunSuite {
  test("successful commands and positive timeout validation") {
    val dir = Files.createTempDirectory("b4smt-process-success-")
    val log = dir.resolve("run.log")
    StandaloneTest.run(Seq("sh", "-c", "printf success"), dir, log, 5)
    assert(Files.readString(log) == "success")
    for (timeout <- Seq(0L, -1L)) {
      intercept[IllegalArgumentException] {
        StandaloneTest.run(Seq("sh", "-c", "exit 0"), dir, log, timeout)
      }
    }
  }

  test("nonzero exits retain command, status, log path and output") {
    val dir = Files.createTempDirectory("b4smt-process-failure-")
    val log = dir.resolve("run.log")
    val failure = intercept[AssertionError] {
      StandaloneTest.run(
        Seq("sh", "-c", "echo diagnostic; exit 7"),
        dir,
        log,
        5,
      )
    }
    assert(failure.getMessage.contains("exit 7"))
    assert(failure.getMessage.contains("exited 7"))
    assert(failure.getMessage.contains(log.toString))
    assert(failure.getMessage.contains("diagnostic"))
  }

  test("timeout and interruption kill the process and its child") {
    for (interrupt <- Seq(false, true)) {
      val dir = Files.createTempDirectory("b4smt-process-cleanup-")
      val log = dir.resolve("run.log")
      val pids = dir.resolve("pids")
      val command = Seq("sh", "-c", "sleep 60 & echo $$ $! > pids; wait")
      val failure = new AtomicReference[Throwable]()
      val interrupted = new AtomicBoolean(false)
      val worker = new Thread(() => {
        try StandaloneTest.run(command, dir, log, if (interrupt) 30 else 1)
        catch {
          case e: Throwable =>
            failure.set(e)
            interrupted.set(Thread.currentThread().isInterrupted)
        }
      })
      worker.setDaemon(true)
      worker.start()
      var handles = Vector.empty[ProcessHandle]
      try {
        val deadline = System.nanoTime() + 5000000000L
        while (
          (!Files.exists(pids) || Files.size(pids) == 0) &&
          System.nanoTime() < deadline
        ) Thread.sleep(10)
        assert(Files.exists(pids), "command did not start")
        handles = Files.readString(pids).trim.split(" ").toVector.map { pid =>
          ProcessHandle.of(pid.toLong).orElseThrow()
        }
        assert(handles.size == 2)
        if (interrupt) worker.interrupt()
        worker.join(6000)
        assert(
          !worker.isAlive,
          "runner did not finish within its cleanup bound",
        )
        if (interrupt) {
          assert(failure.get().isInstanceOf[InterruptedException])
          assert(interrupted.get(), "runner must restore the interrupt flag")
        } else {
          assert(failure.get().isInstanceOf[AssertionError])
          assert(failure.get().getMessage.contains("timed out after 1 seconds"))
          assert(failure.get().getMessage.contains(command.mkString(" ")))
          assert(failure.get().getMessage.contains(log.toString))
        }
        assert(handles.forall(!_.isAlive), "parent or child survived cleanup")
      } finally {
        handles.filter(_.isAlive).foreach(_.destroyForcibly())
        if (worker.isAlive) worker.interrupt()
      }
    }
  }
}
