package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("2nd order polynomial solver should calculate correct delta"){
    val resDelta1 = Polynomial.computeDelta(Var(1), Var(1), Var(1))
    assert(resDelta1() == -3)

    val resSol1 = Polynomial.computeSolutions(Var(1), Var(1), Var(1), resDelta1)
    assert(resSol1() == Set.empty)


    val resDelta2 = Polynomial.computeDelta(Var(2), Var(3), Var(0))
    assert(resDelta2() == 9)

    val resSol2 = Polynomial.computeSolutions(Var(2), Var(3), Var(0), resDelta2)
    assert(resSol2() == Set(0, -1.5))


    val resDelta3 = Polynomial.computeDelta(Var(1), Var(2), Var(1))
    assert(resDelta3() == 0)

    val resSol3 = Polynomial.computeSolutions(Var(-1), Var(1), Var(1), resDelta3)
    assert(resSol3() == Set(0.5, 0.5))


    val resDelta4 = Polynomial.computeDelta(Var(1), Var(0), Var(0))
    assert(resDelta4() == 0)

    val resSol4 = Polynomial.computeSolutions(Var(1), Var(0), Var(0), resDelta4)
    assert(resSol4() == Set(0, 0))


    val resDelta5 = Polynomial.computeDelta(Var(-1), Var(1), Var(1))
    assert(resDelta5() == 5)

  }

  test("Calculator should do basic math"){
    var res = Calculator.eval(Literal(5), Map.empty)
    assert(res == 5)

    res = Calculator.eval(Plus(Literal(5), Literal(5)), Map.empty)
    assert(res == 10)

    res = Calculator.eval(Minus(Literal(5), Literal(5)), Map.empty)
    assert(res == 0)

    res = Calculator.eval(Times(Literal(5), Literal(5)), Map.empty)
    assert(res == 25)

    res = Calculator.eval(Divide(Literal(10), Literal(2)), Map.empty)
    assert(res == 5)

    val refMap = Map[String, Signal[Expr]]("a" -> Var(Literal(2)))

    res = Calculator.eval(Divide(Literal(10), Ref("a")), refMap)
    assert(res == 5)

  }

  test("Calculator should fail with 'NaN' when a reference could not be fulfilled or is cyclic") {
    val namedExpressions = Map[String, Signal[Expr]](
      "a" -> Var(Ref("b")),
      "b" -> Var(Ref("c")),
      "c" -> Var(Ref("a")),
      "d" -> Var(Ref("d")),
      "e" -> Var(Literal(4)),
      "f" -> Var(Ref("e")),
      "g" -> Var(Ref("f")),
      "h" -> Var(Ref("g")),
      "i" -> Var(Ref("h")),
      "j" -> Var(Ref("i")),
      "k" -> Var(Ref("j")),
      "l" -> Var(Ref("k")),
      "m" -> Var(Ref("l")),
      "n" -> Var(Ref("m"))
    )

    var res = Calculator.eval(Ref("a"), namedExpressions)
    assert(res.isNaN)

    res = Calculator.eval(Ref("d"), namedExpressions)
    assert(res.isNaN)

    res = Calculator.eval(Ref("n"), namedExpressions)
    assert(res == 4)

    val result = Calculator.computeValues(namedExpressions)
    assert(result("a")().isNaN)
    assert(result("b")().isNaN)
    assert(result("c")().isNaN)
    assert(result("d")().isNaN)
    assert(result("e")() == 4)
    assert(result("f")() == 4)
    assert(result("g")() == 4)
    assert(result("h")() == 4)
    assert(result("i")() == 4)
    assert(result("j")() == 4)
    assert(result("k")() == 4)
    assert(result("l")() == 4)
    assert(result("m")() == 4)
    assert(result("n")() == 4)

  }
}
