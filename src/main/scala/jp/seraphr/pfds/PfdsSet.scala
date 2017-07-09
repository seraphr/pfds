package jp.seraphr.pfds

/**
  */
trait PfdsSet {
  type Elem <: Ordered[Elem]
  type Set

  val empty: Set
  val insert: (Elem, Set) => Set
  val member: (Elem, Set) => Boolean
}

trait TreeSet extends PfdsSet {
  sealed trait Tree
  case object E extends Tree
  case class T(l: Tree, e: Elem, r: Tree) extends Tree

  type Set = Tree

  override val empty: Set = E


  /** 演習問題2.2  */
  private def memberImpl(aElem: Elem, aSet: Set, aCandidate: Option[Elem]): Boolean = (aElem, aSet) match {
    case (x, E) => aCandidate.fold(false)(_.compare(x) == 0)
    case (x, T(a, y, b)) =>
      if(x <= y) memberImpl(x, a, Some(y))
      else memberImpl(x, b, aCandidate)
  }

  override val member: (Elem, Set) => Boolean = memberImpl(_, _, None)

  case object SameElementException extends RuntimeException

  /** 演習問題2.3  */
  private def insertImpl(aElem: Elem, aSet: Set): Set = (aElem, aSet) match {
    case (x, E) => T(E, x, E)
    case (x, s@T(a, y, b)) =>
      if (x < y) T(insertImpl(x, a), y, b)
      else if (y < x) T(a, y, insertImpl(x, b))
      else throw SameElementException
  }

  override val insert: (Elem, Set) => Set = (aElem, aSet) => try insertImpl(aElem, aSet) catch {
    case SameElementException => aSet
  }
}