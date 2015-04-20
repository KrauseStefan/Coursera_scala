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
//      println("Success")
      return true
    }
    if(min > findMin(h)){
//      println("failed " + min + " > " + findMin(h))
      return false
    }
//    print(findMin(h) + ", ")

    isSorted_rec(findMin(h), deleteMin(h))
  }

  def isSorted_max(h: H, max: Int): Boolean = {
    if(isEmpty(h))
      true
    else
      isSorted_rec_max(findMin(h), deleteMin(h), max - 1)
  }

  def isSorted_rec_max(min: A, h: H, max: Int): Boolean = {
    if(isEmpty(h)) {
      //      println("Success")
      if(max == 0)
        return true
      else
        return false
    }
    if(min > findMin(h)){
      //      println("failed " + min + " > " + findMin(h))
      return false
    }
    //    print(findMin(h) + ", ")

    isSorted_rec_max(findMin(h), deleteMin(h), max -1)
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val j = insert(b, h)
    findMin(j) == (if(a > b) b else a)
  }

  property("min3") = forAll { (a: Int, b: Int, c: Int) =>
    var h = insert(a, empty)
    h = insert(b, h)
//    h = insert(c, h)

    val x = findMin(h)
    h = deleteMin(h)

    val y = findMin(h)
    h = deleteMin(h)


/*
    val z = findMin(h)
    h = deleteMin(h)
*/

    if(isEmpty(h)){
      if(x == y && a == b)
        true
      else if(a > b && b == x && a == y)
        true
      else if(a < b && b == y && a == x)
        true
      else
        false
    }else{
      false
    }
  }

  property("sorted_fixed1") = forAll { (a: Int, b: Int, c: Int) =>
    var h = insert(a, empty)
    h = insert(b, h)
    h = insert(c, h)

    isSorted_max(h, 3)
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

  property("sorted3") = forAll { (h1: H, h2: H, h3: H) =>
    var h = meld(h1, h2)
    h = meld(h, h3)
    isSorted(h)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
