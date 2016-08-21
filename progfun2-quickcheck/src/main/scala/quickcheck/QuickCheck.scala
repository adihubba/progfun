package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.math.max
import scala.math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    number <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(number, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  property("isSorted") = forAll { (h: H) =>
    def isSorted(number: A, heap: H): Boolean = {
      if (isEmpty(heap))
        true
      else if (number > findMin(heap))
        false
      else {
        isSorted(findMin(heap), deleteMin(heap))
      }
    }
    isSorted(findMin(h), deleteMin(h))
  }

  property("multi delete") = forAll { (a: Int, b: Int, c: Int) =>
    findMin(deleteMin(deleteMin(insert(c, insert(b, insert(a, empty)))))) == max(max(a, b), c)
  }

  //  property("gen1") = forAll { (h: H) =>
  //    val m = if (isEmpty(h)) 0 else findMin(h)
  //    findMin(insert(m, h)) == m
  //  }

  //  property("min1") = forAll { a: Int =>
  //    val h = insert(a, empty)
  //    findMin(h) == a
  //  }

  //  property("insert 2 in empty") = forAll { (a: Int, b: Int) =>
  //    findMin(insert(b, insert(a, empty))) == (a min b)
  //  }

  //  property("insert and delete in empty") = forAll { a: Int =>
  //    isEmpty(deleteMin(insert(a, empty)))
  //  }

  //  property("minimum of merge") = forAll { (a: H, b: H) =>
  //    findMin(meld(a, b)) == (min(findMin(a), findMin(b)))
  //  }

}
