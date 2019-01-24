import mill._
import mill.define.TaskModule
import mill.modules.Jvm
import mill.scalalib._
import mill.scalalib.scalafmt._

object backend extends ScalaModule with ScalafmtModule with ParadiseModule {

  override def scalacOptions = Seq("-Ypartial-unification")

  override def mainClass = Some("com.github.szabba.todomvc.replicated.Main")

  override def ivyDeps = Agg(
    ivy"org.http4s::http4s-blaze-server::${versions.http4s}",
    ivy"org.http4s::http4s-circe::${versions.http4s}",
    ivy"org.http4s::http4s-dsl::${versions.http4s}",
    ivy"org.http4s::http4s-server::${versions.http4s}",
    ivy"io.circe::circe-core::${versions.circe}",
    ivy"io.circe::circe-parser::${versions.circe}",
    ivy"io.circe::circe-generic::${versions.circe}")

  object versions {
    val http4s = "0.18.21"
    val circe = "0.9.3"
  }

  object test extends Tests with ParadiseModule {

    override def testFrameworks = Seq("org.scalatest.tools.Framework")

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.5",
      ivy"org.scalacheck::scalacheck::1.14.0")
  }

}

trait ParadiseModule extends ScalaModule {
  override def scalaVersion = "2.12.8"

  val paradiseVersion = "2.1.1"

  override def compileIvyDeps =
    super.compileIvyDeps() ++ Agg(ivy"org.scalamacros::paradise::${paradiseVersion}")


  override def scalacPluginIvyDeps =
    super.scalacPluginIvyDeps() ++ Agg(ivy"org.scalamacros::paradise::${paradiseVersion}")

  override def mapDependencies = T.task { d: coursier.Dependency =>
    if (d.module.name == "paradise_2.12")
      d.copy(module = d.module.copy(name = "paradise_2.12.8"))
    else
      d
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