package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var currentSeq = 0

  val persistance = context.actorOf(persistenceProps)


  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  arbiter ! Join

  def receive = {
    case JoinedPrimary   =>
      context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Replicas(replicas) =>
      val currentReplicas = secondaries.keys.toSet
      val newReplicas = replicas -- currentReplicas
      val deletedReplicas = currentReplicas -- replicas

      deletedReplicas.foreach(replica => {
        val replicator = secondaries(replica)
        replicator ! PoisonPill
        secondaries -= replica
        replicators -= replicator
      })

      newReplicas.foreach(replica => {
        val replicator = context.actorOf(Replicator.props(replica))
        replicators += replicator
        secondaries.updated(replica, replicator)
        kv.foreach {
          case (key, value) => {
            replicator ! Replicate(key, Some(value), 0)
          }
        }
      })

    case Insert(key, value, id) =>
      kv = kv.updated(key, value)

      persistance ! Persist(key, Some(value), id)
//      context.sender() ! OperationAck(operation.id)
    case Persisted(key, id) =>
      context.sender() ! OperationAck(id)
    case Remove(key, id) =>
      persistance ! Persist(key, None, id)
//      kv -= operation.key
      context.sender() ! OperationAck(id)
    case Get(key, id) =>
      context.sender() ! GetResult(key, kv.get(key), id)
  }


  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Snapshot(key, valueOption, seq) =>
      if(currentSeq == seq){
        valueOption match {
          case Some(value) => kv = kv.updated(key, value)
          case None => kv -= key
        }
        currentSeq += 1
      }

      if(seq < currentSeq){
        sender() ! SnapshotAck(key, seq)
      }
    case Get(key, id) =>
      context.sender() ! GetResult(key, kv.get(key), id)
  }

}

