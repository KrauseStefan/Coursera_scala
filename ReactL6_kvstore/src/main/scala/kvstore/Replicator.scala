package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import akka.util.Timeout
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.pattern.{AskTimeoutException, ask, pipe}

import scala.language.postfixOps
import akka.actor.Status.Failure

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]

  implicit val timeout = Timeout(50 milli)

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case operation: Replicate => {
      val seq = nextSeq
      acks = acks.updated(seq, (context.sender(), operation))
      val msg = Snapshot(operation.key, operation.valueOption, seq)
      self ! msg
    }
    case snapshot: Snapshot => {
      (replica ? snapshot).onFailure({
        case t: AskTimeoutException =>
          if(acks.contains(snapshot.seq))
            self ! snapshot
      })
    }
    case operation: SnapshotAck => {
      acks(operation.seq) match {
        case (sender, replicate) => {
          sender ! Replicated(operation.key, replicate.id)
          acks -= operation.seq
        }
      }
    }
  }
}
