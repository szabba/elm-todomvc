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

object frontend extends ElmModule {
}

trait ElmModule extends mill.Module with TaskModule {

  override def defaultCommandName = "compile"

  def compile = T {
    val arguments = List("elm", "make", "--output=elm.js", "src/Main.elm")
    val env = Map.empty[String, String]
    Jvm.baseInteractiveSubprocess(arguments, env, millSourcePath)
  }
}