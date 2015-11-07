package kvstore

import akka.actor.{Props, Actor}
import kvstore.Replica._
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger

object Persistence {
  case class Persist(key: String, valueOption: Option[String], id: Long)
  case class Persisted(key: String, id: Long)

  class PersistenceException extends Exception("Persistence failure")

  def props(flaky: Boolean): Props = Props(classOf[Persistence], flaky)
}

class Persistence(flaky: Boolean) extends Actor {
  import Persistence._

  var kv = Map.empty[String, String]

  def receive = {
    case Persist(key, valueOption, id) =>
      if (!flaky || Random.nextBoolean()){
        valueOption match {
          case Some(value) =>
            kv = kv.updated(key, value)
          case None =>
            kv -= key
        }
        sender ! Persisted(key, id)

      }
      else throw new PersistenceException

    case Get(key, id) =>
      sender ! GetResult(key, kv.get(key), id)



  }


}
