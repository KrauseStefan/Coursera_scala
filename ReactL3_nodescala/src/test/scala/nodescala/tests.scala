package nodescala

import java.util.NoSuchElementException
import java.util.concurrent.ExecutionException

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.all should be completed with the right results in the right order") {
    val testList = (0 to 10).toList

    val futures = for (data <- testList)
      yield Future[Int](data)

    assert(Await.result(Future.all(futures), 1 second) == testList)
  }

  test("Future.all should return a failed future if a element fails 1") {
    val exception = new ExecutionException(new Throwable("test"))
    val failedFuture = Future.failed(exception)

    val futures = for (data <- 0 to 10)
      yield Future[Int](data)

    val failedFutures = failedFuture +: futures

    try {
      Await.result(Future.all(failedFutures.toList), 1 second)
      assert(false)
    } catch {
      case t1: TimeoutException => assert(false)
      case t2: ExecutionException => // ok!
    }
  }

  test("Future.all should return a failed future if a element fails 2") {
    val exception = new ExecutionException(new Throwable("test"))
    val failedFuture = Future.failed(exception)

    val futures = for (data <- 0 to 10)
      yield Future[Int](data)

    val failedFutures = futures :+ failedFuture

    try {
      Await.result(Future.all(failedFutures.toList), 1 second)
      assert(false)
    } catch {
      case t1: TimeoutException => assert(false)
      case t2: ExecutionException => // ok!
    }
  }

  test("Future.all should never resolve if none are completed") {
    val futures = for (x <- 0 to 10)
      yield Future.never[Int]

    try {
      Await.result(Future.all(futures.toList), 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.any should return the first resolved future from the list 1") {
    val futures = List.empty :+ Future {
      10
    }

    assert(Await.result(Future.any(futures), 1 second) == 10)
  }

  test("Future.any should return the first resolved future from the list 2") {
    val futures = List.empty :+ Future {
      10
    } :+ Future {
      12
    }

    assert(Await.result(Future.any(futures), 1 second) == 10)
  }

  test("Future.any should return the first resolved future from the list 3") {
    val futures = List.empty :+ Future.never :+ Future {
      12
    }

    assert(Await.result(Future.any(futures), 1 second) == 12)
  }

  test("Future.any should return a failed future if a element fails") {
    val testList = (0 to 10).toList

    val exception = new ExecutionException(new Throwable("test"))
    val failedFuture = Future.failed(exception)

    val futures = for (data <- testList)
      yield Future[Int](data)

    val failedFutures = futures :+ failedFuture

    try {
      Await.result(Future.all(failedFutures), 1 second)
      assert(false)
    } catch {
      case t1: TimeoutException => assert(false)
      case t2: ExecutionException => // ok!
    }
  }

  test("Future.any should never resolve if none are completed") {
    val futures = for (x <- 0 to 10)
      yield Future.never[Int]

    try {
      Await.result(Future.any(futures.toList), 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.delay should not resolve before timeout has been reached") {
    try {
      Await.result(Future.delay(1 second), 100 milli)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.delay should resolve after timeout has been reached") {
    try {
      Await.result(Future.delay(100 milli), 500 milli)
      assert(true)
    } catch {
      case t: TimeoutException => assert(false)
    }
  }


  test("Future.run should allow stopping the computation") {
    var isWorking = false
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          isWorking = true
        }
        isWorking = false
      }
    }
    val done = Future.delay(100 milli).andThen({
      case _ => working.unsubscribe()
    })

    try {
      assert(isWorking == true)
      Await.result(done, 200 milli)
      Thread.sleep(100)
      assert(isWorking == false)
    } catch {
      case t: TimeoutException => assert(false)
    }

  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("Future.now should return the result of the promise immediately") {
    val f = Future{10}

    assert(f.now == 10)
  }

  test("Future.now should throw an exception if future is not resolved") {
    val p = Promise[Int]()

    try{
      p.future.now
      assert(false)
    }catch {
      case t: NoSuchElementException => assert(true)
      case _: Throwable => assert(false)
    }
  }

  test("Future.continueWith should map the current feature into a new feature of a different type") {
    val f1 = Future[String]{"234"}

    val f2 = f1.continueWith((f) => {
      f.now.toInt
    })

    assert(Await.result(f2, 1 second) == 234)
  }

  test("Future.continueWith should handle exceptions1") {
    val f1 = Future[String]{"234"}

    val f2 = f1.continueWith((f) => {
      throw new IllegalStateException
      f.now.toInt
    })

    try{
      Await.result(f2, 1 second)
      assert(false)
    }catch {
      case e: IllegalStateException => assert(true)
      case t: Throwable => assert(false)
    }
  }

  test("Future.continueWith should handle exceptions2") {
    val f1 = Future.failed[String](new IllegalStateException)

    val f2 = f1.continueWith((f) => {
      f.now.toInt
    })

    try{
      Await.result(f2, 1 second)
      assert(false)
    }catch {
      case e: IllegalStateException => assert(true)
      case t: Throwable => assert(false)
    }
  }

  test("Future.continue should map the current feature into a new feature of a different type") {
    val f1 = Future[String]{"234"}

    val f2 = f1.continue((f) => {
      f.get.toInt
    })

    assert(Await.result(f2, 1 second) == 234)
  }

  test("Future.continue should handle exceptions1") {
    val f1 = Future[String]{"234"}

    val f2 = f1.continue((f) => {
      throw new IllegalStateException
      f.get.toInt
    })

    try{
      Await.result(f2, 1 second)
      assert(false)
    }catch {
      case e: IllegalStateException => assert(true)
      case t: Throwable => assert(false)
    }
  }

  test("Future.continue should handle exceptions2") {
    val f1 = Future.failed[String](new IllegalStateException)

    val f2 = f1.continue((f) => {
      f.get.toInt
    })

    try{
      Await.result(f2, 1 second)
      assert(false)
    }catch {
      case e: IllegalStateException => assert(true)
      case t: Throwable => assert(false)
    }
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()

    def write(s: String) {
      response += s
    }

    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




