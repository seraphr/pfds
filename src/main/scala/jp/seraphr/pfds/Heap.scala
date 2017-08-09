package jp.seraphr.pfds

/**
 */
trait Heap {
  protected val elem: PfdsOrdered

  type Heap

  val empty: Heap
  val isEmpty: Heap => Boolean
  val insert: (elem.T, Heap) => Heap
  val merge: (Heap, Heap) => Heap
  val findMin: Heap => elem.T
  val deleteMin: Heap => Heap
  val fromList: List[elem.T] => Heap

  case object EmptyException extends RuntimeException
}

object LeftistHeap {
  def apply(aElem: PfdsOrdered) = new LeftistHeap {
    val elem: aElem.type = aElem
  }
}

abstract class LeftistHeap extends Heap {
  sealed trait Heap
  case object E extends Heap
  case class T(rank: Int, v: elem.T, left: Heap, right: Heap) extends Heap

  override val empty: Heap = E
  override val isEmpty: Heap => Boolean = _ == empty
  def rank(h: Heap): Int = h match {
    case E             => 0
    case T(r, _, _, _) => r
  }
  private def makeT(x: elem.T, a: Heap, b: Heap): Heap = {
    if (rank(a) >= rank(b)) T(rank(b) + 1, x, a, b)
    else T(rank(a) + 1, x, b, a)
  }
  override val merge: (Heap, Heap) => Heap = {
    case (h, E) => h
    case (E, h) => h
    case (h1 @ T(_, x, a1, b1), h2 @ T(_, y, a2, b2)) =>
      if (elem.leq(x, y)) makeT(x, a1, merge(b1, h2))
      else makeT(y, a2, merge(h1, b2))
  }

  private def single(a: elem.T): Heap = T(1, a, E, E)
  //  override val insert: (elem.T, Heap) => Heap = (e, h) => merge(single(e), h)

  /** 演習問題 3.2 */
  override val insert: (elem.T, Heap) => Heap = {
    case (x, E) => single(x)
    //    case (x, T(_, y, E, E)) =>
    //      if (elem.leq(x, y)) T(1, x, single(y), E)
    //      else T(1, y, T(1, x, E, E), E)
    //    case (x, T(_, y, a, E)) =>
    //      if (elem.leq(x, y)) makeT(x, a, single(y))
    //      else makeT(y, a, single(x))
    //    case (x, T(_, y, E, a)) =>
    //      if (elem.leq(x, y)) makeT(x, a, single(y))
    //      else makeT(y, a, single(x))
    case (x, T(_, y, a, b)) =>
      val (tTop, tInsertV) = if (elem.leq(x, y)) (x, y) else (y, x)
      makeT(tTop, insert(tInsertV, b), a)
  }

  override val findMin: (Heap) => elem.T = {
    case E             => throw EmptyException
    case T(_, x, _, _) => x
  }
  override val deleteMin: (Heap) => Heap = {
    case E             => throw EmptyException
    case T(_, _, a, b) => merge(a, b)
  }

  /** 演習問題 3.3 */
  private def mergeList(aList: List[Heap]): List[Heap] = aList match {
    case Nil              => List(empty)
    case r @ List(_)      => r
    case List(e1, e2)     => List(merge(e1, e2))
    case e1 :: e2 :: tail => mergeList(merge(e1, e2) :: mergeList(tail))
  }

  override val fromList: List[elem.T] => Heap = aList => mergeList(aList.map(single)).head

  def checkRealRank(h: Heap): Boolean = h match {
    case E => true
    case T(r, _, a, b) =>
      checkRealRank(a) && checkRealRank(b) && (rank(a) min rank(b)) + 1 == r
  }
  def checkMin(h: Heap, aMinValue: elem.T): Boolean = h match {
    case E => true
    case T(_, x, a, b) =>
      elem.leq(aMinValue, x) && checkMin(a, x) && checkMin(b, x)
  }

  def checkLeftist(h: Heap): Boolean = h match {
    case E => true
    case h @ T(r, x, a, b) =>
      val tLeftRank = rank(a)
      val tRightRank = rank(b)
      tLeftRank >= tRightRank && checkLeftist(a) && checkLeftist(b)
  }

  def isLeftist(h: Heap): Boolean = {
    val tCheckRealRank = checkRealRank(h)

    lazy val tCheckLeftist = checkLeftist(h)
    lazy val tCheckMin =
      h match {
        case E             => true
        case T(_, x, _, _) => checkMin(h, x)
      }

    //    println(s"realRank => ${tCheckRealRank}")
    //    println(s"leftist => ${tCheckLeftist}")
    //    println(s"min => ${tCheckMin}")
    tCheckRealRank && tCheckLeftist && tCheckMin
  }
}