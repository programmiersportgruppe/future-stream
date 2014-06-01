package org.programmiersportgruppe.future

import FutureStream._
import scala.concurrent.Future

class FutureStreamTest extends FutureTest {


    test("Can create future list with cons and get sequence") {
        val fList: FutureStream[String] = "1" :: "2" :: "3" :: FutureStream.empty

        assertResult(Seq("1", "2", "3"))(await(fList.sequence()))
    }


    test("can first 3 elements") {
        val fiveNumbers: FutureStream[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: FutureStream.empty

        val firstThreeNumbers = fiveNumbers.take(3)

        assertResult(Seq(1, 2, 3))(await(firstThreeNumbers.sequence()))
    }


    test("take will get the first 5 elements of induced list") {
        val naturalNumbers: FutureStream[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        val firstFiveNumbers = naturalNumbers.take(5)

        assertResult(Seq(1, 2, 3 ,4 ,5))(await(firstFiveNumbers.sequence()))
    }

    test("take(0) will get empty future list") {
        val naturalNumbers: FutureStream[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        val firstZeroNumbers = naturalNumbers.take(0)

        assertResult(FutureStream.empty)(firstZeroNumbers)
    }


    test("map will apply function lazily") {
        val naturalNumbers: FutureStream[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        val squares = naturalNumbers.map(i => i * i)

        assertResult(Seq(1, 4, 9))(await(squares.take(3).sequence()))
    }

    test("filter works lazily") {
        val naturalNumbers: FutureStream[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        // This change in type is necessary, because we don't now whether the first value is available yet
        // It would nice to find a way to treat Future[FutureListDoublePrime[T]] and FutureListDoublePrime[T] uniformly
        val evenNumbers: FutureStream[Int] = naturalNumbers.filter(_ % 2 == 0)

        assertResult(Seq(2, 4, 6))(await(evenNumbers.take(3).sequence()))
    }

    test("flatten works lazily") {
        val naturalNumbers: FutureStream[Int] = induce(1, (i: Int) => Future.successful(Some(i + 1)))

        val pages: FutureStream[Seq[Int]] = naturalNumbers.map(i => Seq(i, i + 1))

        // Again flattening yields awkward type
        val flattenedList: FutureStream[Int] = pages.flatten

        assertResult(Seq(1, 2, 2, 3))(await(flattenedList.take(4).sequence()))
    }

}
