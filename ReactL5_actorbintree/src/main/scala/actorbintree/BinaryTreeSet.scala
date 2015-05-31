/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor
import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  var pendingQueue = Queue.empty[Operation]

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case operation: Operation =>
      root ! operation
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
  }

  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished =>
      context.watch(root)
      root ! PoisonPill
    case (o: Operation) =>
      pendingQueue = pendingQueue :+ o
    case GC =>
    //Ignored
    case t: Terminated =>
      for (operation <- pendingQueue) {
        newRoot ! operation
      }

      root = newRoot
      context.become(normal)
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}


class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def receive = normal

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case operation: Operation =>
      if(operation.elem.equals(elem)){
        operation match {
          case o: Insert =>
            removed = false
            o.requester ! OperationFinished(o.id)
          case o: Remove =>
            removed = true
            o.requester ! OperationFinished(o.id)
          case o: Contains =>
            o.requester ! ContainsResult(o.id, !removed)
        }
      }else{
        val position = if (operation.elem < elem) Left else Right
        subtrees.get(position) match {
          case Some(subNode) => subNode ! operation
          case None => operation match {
            case o: Insert =>
              subtrees = subtrees.updated(position, context.actorOf(BinaryTreeNode.props(o.elem, initiallyRemoved = false)))
              o.requester ! OperationFinished(o.id)
            case o: Remove =>
              o.requester ! OperationFinished(o.id)
            case o: Contains =>
              o.requester ! ContainsResult(o.id, result = false)
          }
        }
      }
    case copyTo: CopyTo =>
      val refs = subtrees.values

      for( ref <- refs){
        ref ! CopyTo(copyTo.treeNode)
      }

      if(removed && refs.isEmpty){
        context.parent ! CopyFinished
      }else{
        if(!removed) {
          copyTo.treeNode ! Insert(self, 0, elem)
        }
        context.become(copying(refs.toSet, removed))
      }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case o: OperationFinished =>
      if(insertConfirmed){
        throw new Exception("This should never ever happon")
      }
      if(expected == Set.empty){
        context.parent ! CopyFinished
      }else{
        context.become(copying(expected, insertConfirmed = true))
      }
    case CopyFinished =>
      if(expected.isEmpty){
        throw new Exception("This should never ever happon")
      }

      if(expected.size <= 1 && insertConfirmed){
        context.parent ! CopyFinished
      }else{
        context.become(copying(expected - sender(), insertConfirmed))
      }
  }


}
