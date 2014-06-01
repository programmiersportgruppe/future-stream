package org.programmiersportgruppe.future

import org.scalatest.FunSuite
import scala.concurrent.{Await, Future}

abstract class FutureTest extends FunSuite {
    implicit val globalExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    import scala.concurrent.duration._
    def await[T](f: Future[T]): T = Await.result(f, 1 second)

}
