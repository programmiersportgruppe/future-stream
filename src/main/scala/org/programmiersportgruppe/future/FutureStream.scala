package org.programmiersportgruppe.future

import scala.concurrent.{Future, ExecutionContext}
import scala.collection.GenTraversableOnce

case class TraversalException[T](successfulPrefix: List[T], cause: Throwable) extends RuntimeException(s"Error while traversing list after ${successfulPrefix.size} elements: ${cause.getMessage}", cause)

class FutureStream[+T](lazyValue: () => Future[Option[(T, FutureStream[T])]]) {

    lazy val value = lazyValue()

    def ::[U >: T](x: => U)(implicit executor: ExecutionContext): FutureStream[U] =
        FutureStream(Future(Some((x, this))))

    def sequence()(implicit executor: ExecutionContext): Future[List[T]] = value.flatMap {
        case None => Future.successful(Nil)
        case Some((head, tail)) => tail.sequence().map(head :: _)
    }

    def take(n: Int)(implicit executor: ExecutionContext): FutureStream[T] =
        if (n > 0) FutureStream(value.map {
            case None => None
            case Some((h, tail)) => Some((h, tail.take(n - 1)))
        }) else FutureStream.empty

    def map[U](f: T => U)(implicit executor: ExecutionContext): FutureStream[U] = FutureStream(value.map {
        case None => None
        case Some((h, tail)) => Some((f(h), tail.map(f)))
    })

    def filter(p: T => Boolean)(implicit executor: ExecutionContext): FutureStream[T] = FutureStream(value.flatMap {
        case None => FutureStream.empty.value
        case Some((h, tail)) if p(h) => Future.successful(Some((h, tail.filter(p))))
        case Some((_, tail)) => tail.filter(p).value
    })

    def flatten[B](implicit asTraversable: T => /*<:<!!!*/ GenTraversableOnce[B], executor: ExecutionContext): FutureStream[B] = FutureStream(value.flatMap {
        case None => FutureStream.empty.value
        case Some((h, tail)) => asTraversable(h).foldRight(tail.flatten) {
            case (elem, t) => elem :: t
        }.value
    })

}

object FutureStream {

    private def apply[T](newValue: => Future[Option[(T, FutureStream[T])]]): FutureStream[T] =
        new FutureStream(() => newValue)

    val empty = FutureStream(Future.successful(None))

    def induce[T](head: T, f: (T) => Future[Option[T]])(implicit executor: ExecutionContext): FutureStream[T] =
        FutureStream(Future.successful(Some((head, FutureStream(f(head).flatMap {
            case None => FutureStream.empty.value
            case Some(n) => induce(n, f).value
        })))))

}
