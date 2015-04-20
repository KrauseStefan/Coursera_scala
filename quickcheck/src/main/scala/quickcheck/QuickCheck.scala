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


    /*
    1 < 2 < 3
    1 < 3 > 2
    2 > 1 < 3
    2 < 3 > 1
    3 > 1 < 2
    3 > 2 > 1
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


/*  property("min4") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, empty)
    val i = insert(b, h)
    val j = insert(c, h)

    val x = findMin(j)
    val k = deleteMin(j)

    val y = findMin(k)
    val l = deleteMin(k)

    val z = findMin(k)
    if(x == y && a == b)
      true
    else if(a > b && b == x && a == y)
      true
    else if(a < b && b == y && a == x)
      true
    else
      false
  }*/


  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    val j = deleteMin(h)
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
    findMin(insert(m, h))==m
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
