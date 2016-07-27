package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    f <- arbitrary[Boolean]
    h <- oneOf(const(empty), genHeap)
  } yield if (f) h else insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (x: A, y: A) =>
    val q = insert(x, insert(y, empty))
    val m = findMin(q)
    if (ord.lteq(x, y)) x == m else y == m
  }

  property("gen3") = forAll { (x: A) =>
    isEmpty(deleteMin(insert(x, empty)))
  }

  property("gen4") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    val m = if (isEmpty(melded)) 0 else findMin(melded)
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    (m == m1) || (m == m2)
  }

  property("gen5") = forAll { (h: H) =>
    (!isEmpty(h)) ==> {
      def checkSorted(min: A, remain: H): Boolean =
        if (!isEmpty(remain)) {
          val mr = findMin(remain)
          (ord.lteq(min, mr)) && checkSorted(mr, deleteMin(remain))
        } else true
      val m = findMin(h)
      checkSorted(m, deleteMin(h))
    }
  }
  
  property("gen6") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val melded = meld(h1, h2)
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      
      def find(tm: A, h: H): Boolean = {
        val m = findMin(h)
        if (tm == m) true
        else find(tm, deleteMin(h))
      }
      find(m1, melded)
      find(m2, melded)
    }
  }
  
}
