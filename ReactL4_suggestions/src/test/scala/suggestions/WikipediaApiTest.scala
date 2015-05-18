package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable.just("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi sanitize should replace space with underscore") {

    val testDataInput = Observable.just("erik", "erik meijer", "martin")
    val testDataOutput = List("erik", "erik_meijer", "martin")

    val testData = testDataInput.sanitized.toBlocking.toList

    assert(testData.equals(testDataOutput), testData)
  }

  test("WikipediaApi recovered should wrap every element in a Try") {

    val exception = new Exception
    val testDataInput = Observable.just("erik", "erik meijer", "martin", Unit, "martin")
    val testDataOutput = List(Success("erik"), Success("erik meijer"), Success("martin"), Failure(exception))


    val testData = testDataInput.map(x => {
      if(x == Unit){
        throw exception
      }
      x
    }).recovered.toBlocking.toList

    assert(testData.equals(testDataOutput), testData)
  }

  test("WikipediaApi timedOut should automatically close the observable stream") {

    val testDataInput = Observable.interval(180 millisecond)
    val testDataOutput = List(0, 1, 2, 3, 4)
    var completed = false

    val testData = testDataInput.timedOut(1).doOnCompleted({completed = true})

    assert(testData.toBlocking.toList.equals(testDataOutput), testData)
    assert(completed == true, ": Stream was not closed")
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable.just(1, 2, 3)
    val remoteComputation = (n: Int) => Observable.just(0 to n : _*)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

}
