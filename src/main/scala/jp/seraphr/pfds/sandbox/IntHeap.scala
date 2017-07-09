package jp.seraphr.pfds.sandbox

import jp.seraphr.pfds.{LeftistHeap, PfdsOrdered}
import jp.seraphr.pfds.sandbox.IntHeap.IntOrdered

/**
  */
object IntHeap extends App{
  object IntOrdered extends PfdsOrdered {
    override type T = Int
    override def eq(a: Int, b: Int): Boolean = a == b
    override def leq(a: Int, b: Int): Boolean = a <= b
    override def lt(a: Int, b: Int): Boolean = a < b
  }

  val m = LeftistHeap(IntOrdered)
  val tHeap = scala.util.Random.shuffle((0 to 10).toList).foldLeft(m.empty)((h, v) => m.insert(v, h))
  println(m.findMin(tHeap))
  println(m.findMin(m.deleteMin(tHeap)))
  println(tHeap)
  println(m.isLeftist(tHeap))
}
