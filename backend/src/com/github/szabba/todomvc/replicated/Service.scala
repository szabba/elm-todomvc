package com.github.szabba.todomvc.replicated

import com.github.szabba.todomvc.replicated.model.TodoList
import cats.effect.IO
import org.http4s.HttpService
import org.http4s.dsl.io._
import org.http4s.circe._
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object Service {

  private implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  private var todoList = TodoList.empty

  val service: HttpService[IO] = HttpService[IO] {
    case GET -> Root / "todo" => {
      Ok(todoList.asJson)
    }

    case request @ PUT -> Root / "todo" =>
      for {
        incoming <- request.decodeJson[TodoList]
        _ <- IO {
          todoList = todoList.merge(incoming)
        }
        response <- Ok(todoList.asJson)
      } yield {
        response
      }
  }
}
