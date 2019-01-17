import mill._
import mill.scalalib._
import mill.scalalib.scalafmt.ScalafmtModule

object backend extends ScalaModule with ScalafmtModule {

  override def scalaVersion = "2.12.8"

  object test extends Tests {

    override def testFrameworks = Seq("org.scalatest.tools.Framework")

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.5",
      ivy"org.scalacheck::scalacheck::1.14.0")
  }
}