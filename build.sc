import mill._
import mill.define.TaskModule
import mill.modules.Jvm
import mill.scalalib._
import mill.scalalib.scalafmt._

object backend extends ScalaModule with ScalafmtModule {

  override def scalaVersion = "2.12.8"

  object test extends Tests {

    override def testFrameworks = Seq("org.scalatest.tools.Framework")

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.5",
      ivy"org.scalacheck::scalacheck::1.14.0")
  }

}

object frontend extends ElmModule

trait ElmModule extends mill.Module with TaskModule {

  override def defaultCommandName = "compile"

  def compile = T {
    val srcs = allSources()
    val commandPrefix = Seq("elm", "make", "--output=elm.js")
    val elmFiles = srcs.map(_.path.relativeTo(millSourcePath).toString)
    val args = commandPrefix ++ elmFiles
    Jvm.baseInteractiveSubprocess(args, Map.empty, millSourcePath)
  }

  def allSources = T.sources {
    os.walk(millSourcePath)
      .filter(_.toString.endsWith(".elm"))
      .map(x => x)
      .map(PathRef(_))
  }
}