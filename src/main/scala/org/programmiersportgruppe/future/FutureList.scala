package org.programmiersportgruppe.future

import scala.concurrent.{Await, Future}

//TODO this must go
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.GenTraversableOnce
import scala.concurrent.Future

object FutureList {
    def induce[T](head: T, f: (T) => Future[Option[T]]): FutureList[T] =

        Cons(head, f(head)
            .map((nHead: Option[T]) => nHead.map(induce(_, f)).getOrElse(FNil)))


    def sequence[T](in: FutureList[T]): Future[Seq[T]] = in match {
        case FNil => Future(Seq())
        case fList: FutureList[T] => fList.tail.flatMap(sequence(_).map((seq: Seq[T]) => Seq(fList.head) ++ seq))
    }


    def sequence[T](in: Future[FutureList[T]]): Future[Seq[T]] = in.flatMap(sequence(_))
}


trait FutureList[+T] {
    def head: T

    def tail: Future[FutureList[T]]

    def isEmpty: Boolean

    def ::[U >: T](x: U) = Cons(x, Future.successful(this))

    def take(n: Int): FutureList[T] = {
        if (n == 0)
            return FNil
        Cons(head, tail.map(_.take(n - 1)))
    }

    def takeWhile(predicate: T => Boolean): FutureList[T] = {
        if (!predicate(head))
            return FNil
        Cons(head, tail.map(_.takeWhile(predicate)))
    }

    def map[U](f: T => U): FutureList[U] =
        Cons(f(head), tail.map(_.map(f)))


    def filter(predicate: T => Boolean): Future[FutureList[T]] = {
        if (predicate(head))
            Future {
                Cons(head, tail.flatMap(_.filter(predicate)))
            }
        else
            tail.flatMap(_.filter(predicate))
    }

    def flatten[B](implicit asTraversable: T => GenTraversableOnce[B]): Future[FutureList[B]] = {
        val tHead: GenTraversableOnce[B] = asTraversable(head)
        tHead.foldRight(tail.flatMap(_.flatten))((e: B, acc) => Future {
            Cons(e, acc)
        })
    }

    def sequence(): Future[Seq[T]] = this match {
        case FNil => Future(Seq())
        case fList: FutureList[T] => fList.tail.flatMap(_.sequence().map((seq: Seq[T]) => Seq(fList.head) ++ seq))
    }
}

object FNil extends FutureList[Nothing] {
    override def head = ???

    override def tail = ???

    override def isEmpty = true

    override def take(n: Int) = this

    override def takeWhile(predicate: Nothing => Boolean) = this

    override def map[U](f: Nothing => U): FutureList[U] = this

    override def filter(predicate: Nothing => Boolean) = Future.successful(this)

    override def flatten[B](implicit asTraversable: Nothing => /*<:<!!!*/ GenTraversableOnce[B]): Future[FutureList[B]] = Future.successful(this)
}

case class Cons[T](head: T, tail: Future[FutureList[T]]) extends FutureList[T] {
    override def isEmpty = false
}

case class LazyCons[T](head: T, tailFunction: () => Future[FutureList[T]]) extends FutureList[T] {
    override lazy val tail = tailFunction()
    override def isEmpty = false
}


object Testme extends App {

    def sequence[T](in: FutureList[T]): Future[Seq[T]] = in match {
        case FNil => Future(Seq())
        case fList: FutureList[T] => fList.tail.flatMap(sequence(_).map((seq: Seq[T]) => Seq(fList.head) ++ seq))
    }


    def sequence[T](in: Future[FutureList[T]]): Future[Seq[T]] = in.flatMap(sequence(_))


    import scala.concurrent.duration._

    def await[T](f: Future[T]): T = Await.result(f, 10 seconds)

    val s: Seq[String] = "Hello" :: Nil

    val test: FutureList[Int] = 1 :: FNil

    val paginated: FutureList[Seq[Int]] = Seq(1, 2, 3) :: Seq(6, 7, 8) :: FNil

    val sequenced = sequence(paginated)

    println(await(sequenced))
    println(await(sequence(paginated.flatten)))

    val naturals: FutureList[Int] = FutureList.induce(1, (i: Int) => Future {
        Some(i + 1)
    })

    val naturlsLt15 = naturals.takeWhile(_ < 15)
    val even: Future[FutureList[Int]] = naturlsLt15.filter(_ % 2 == 0)


    println(await(sequence(naturals.map(Seq(_)).flatten.map(_.take(10)))))

    val sequenceNaturals = sequence(even)


    println(await(sequenceNaturals))


}
