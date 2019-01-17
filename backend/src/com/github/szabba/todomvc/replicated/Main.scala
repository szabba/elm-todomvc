package com.github.szabba.todomvc.replicated

import cats.effect._
import fs2.StreamApp
import org.http4s.dsl.io._
import org.http4s.server.blaze._
import scala.concurrent.ExecutionContext

object Main extends StreamApp[IO] {

  implicit val ec = ExecutionContext.global

  val service = org.http4s.HttpService[IO] {
    case GET -> Root / "todo" => {
      Ok()
    }

    case POST -> Root / "todo" =>
      Ok()
  }

  val builder = BlazeBuilder[IO]
    .bindHttp(8080, "localhost")
    .mountService(service)

  override def stream(
      args: List[String],
      requestShutdown: IO[Unit]): fs2.Stream[IO, StreamApp.ExitCode] =
    builder.serve
}
