package com.github.szabba.todomvc.replicated

import cats.effect._
import fs2.StreamApp
import org.http4s.server.blaze._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object Main extends StreamApp[IO] {

  private implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  private val builder = BlazeBuilder[IO]
    .bindHttp(8080, "localhost")
    .mountService(Service.service)

  override def stream(
      args: List[String],
      requestShutdown: IO[Unit]): fs2.Stream[IO, StreamApp.ExitCode] =
    builder.serve
}
