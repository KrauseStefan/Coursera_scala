package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  def isSorted(h: H): Boolean = {
    if(isEmpty(h))
      true
    else
      isSorted_rec(findMin(h), deleteMin(h))
  }

  def isSorted_rec(min: A, h: H): Boolean = {
    if(isEmpty(h)) {
      return true
    }
    if(min > findMin(h)){
      return false
    }

    isSorted_rec(findMin(h), deleteMin(h))
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min3") = forAll { (a: Int, b: Int, c: Int) =>
    var h = insert(a, insert(b, insert(c, empty)))

    val listA = Vector(a, b, c).sorted

    val x = findMin(h)
    h = deleteMin(h)

    val y = findMin(h)
    h = deleteMin(h)

    val z = findMin(h)
    h = deleteMin(h)

    val listB = Vector(x,y,z)

    listA.sameElements(listB)
  }

  property("empty") = forAll { a: Int =>
    var h = insert(a, empty)
    h = insert(a, h)

    var j = deleteMin(h)
    j = deleteMin(j)

    isEmpty(j)
  }

  property("sorted1") = forAll { h: H =>
    isSorted(h)
  }

  property("sorted2") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    isSorted(h)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
