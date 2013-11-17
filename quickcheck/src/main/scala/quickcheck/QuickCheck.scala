package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfTwo") = forAll { (a: Int, b: Int) =>
    val min = Math.min(a, b)
    val h = insert(a, insert(b, empty))
    findMin(h) == min
  }

  property("insertAndDelete") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("meldHeaps") = forAll { (h1: H, h2: H) =>
    val meld1 = meld(h1, h2)
    val minH1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(minH1, h2))
    toSeq(Nil, meld1) == toSeq(Nil, meld2)
  }

  property("sortedSequence") = forAll { (h: H) =>
    val sequence = toSeq(Nil, h).reverse
    sequence == sequence.sorted
  }

  property("meldingMinimum") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val min = findMin(h)
    min == findMin(h1) || min == findMin(h2)
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  @tailrec
  final def toSeq(s: List[A], h: H): List[A] = if (isEmpty(h)) {
    s
  } else {
    toSeq(findMin(h) :: s, deleteMin(h))
  }
}
