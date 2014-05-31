package org.programmiersportgruppe.future

import org.scalatest.FunSuite
import FutureList._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class FutureListTest extends FunSuite {


    test("Can create future list with cons and get sequence") {
        val fList: FutureList[String] = "1" :: "2" :: "3" :: FNil

        assertResult(Seq("1", "2", "3"))(await(fList.sequence()))
    }


    test("take will get the first 6 elements") {
        val naturalNumbers: FutureList[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        val firstFiveNumbers = naturalNumbers.take(5)

        assertResult(Seq(1, 2, 3 ,4 ,5))(await(firstFiveNumbers.sequence()))
    }

    test("take(0) will get empty future list") {
        val naturalNumbers: FutureList[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        val firstZeroNumbers = naturalNumbers.take(0)

        assertResult(FNil)(firstZeroNumbers)
    }


    test("map will apply function lazily") {
        val naturalNumbers: FutureList[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        val squares = naturalNumbers.map(i => i * i)

        assertResult(Seq(1, 4, 9))(await(squares.take(3).sequence()))
    }

    test("filter works lazily") {
        val naturalNumbers: FutureList[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        // This change in type is necessary, because we don't now whether the first value is available yet
        // It would nice to find a way to treat Future[FutureList[T]] and FutureList[T] uniformly
        val evenNumbers: Future[FutureList[Int]] = naturalNumbers.filter(_ % 2 == 0)

        assertResult(Seq(2, 4, 6))(await(evenNumbers.map(_.take(3)).flatMap(_.sequence())))
    }

    test("flatten works lazily") {
        val naturalNumbers: FutureList[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        val pages: FutureList[Seq[Int]] = naturalNumbers.map(i => Seq(i, i + 1))

        // Again flattening yields awkward type
        val flattenedList: Future[FutureList[Int]] = pages.flatten

        assertResult(Seq(1, 2, 2, 3))(await(flattenedList.flatMap(_.take(4).sequence())))
    }

    import scala.concurrent.duration._
    def await[T](f: Future[T]): T = Await.result(f, 1 second)

}
