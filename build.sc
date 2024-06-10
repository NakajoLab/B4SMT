// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
// support BSP
import mill.bsp._

val chiselVersion = "6.3.0"
val chiselTestVersion = "6.0.0"

object b4smt_formal extends SbtModule { m =>
  override def millSourcePath = os.pwd / "chisel-formal"
  override def scalaVersion = "2.13.12"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
  )
  override def ivyDeps = Agg(
    ivy"org.chipsalliance::chisel:${chiselVersion}",
    ivy"org.scalatest::scalatest::3.2.16"
  )
}

object b4smt_pext extends SbtModule { m =>
  override def millSourcePath = os.pwd / "pext"
  override def scalaVersion = "2.13.12"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
  )
  override def ivyDeps = Agg(
    ivy"org.chipsalliance::chisel:${chiselVersion}",
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"org.chipsalliance:::chisel-plugin:${chiselVersion}",
  )
}

object b4smt extends SbtModule { m =>
  override def millSourcePath = os.pwd / "b4smt"
  override def mainClass = Some("b4smt.B4SMTCore")
  override def scalaVersion = "2.13.12"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
  )
  override def ivyDeps = Agg(
    ivy"org.chipsalliance::chisel:${chiselVersion}",
  )
  def moduleDeps = Seq(
    b4smt_formal,
    b4smt_pext
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"org.chipsalliance:::chisel-plugin:${chiselVersion}",
  )
  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = m.ivyDeps() ++ Agg(
      ivy"org.scalatest::scalatest::3.2.16",
      ivy"edu.berkeley.cs::chiseltest::${chiselTestVersion}",
    )
  }
}