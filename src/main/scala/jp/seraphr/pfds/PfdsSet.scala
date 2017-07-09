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


  /** 課題2-2  */
  private def memberImpl(aElem: Elem, aSet: Set, aCandidate: Option[Elem]): Boolean = (aElem, aSet) match {
    case (x, E) => aCandidate.fold(false)(_.compare(x) == 0)
    case (x, T(a, y, b)) =>
      if(x <= y) memberImpl(x, a, Some(y))
      else memberImpl(x, b, aCandidate)
  }

  /** 課題2-2  */
  override val member: (Elem, Set) => Boolean = memberImpl(_, _, None)

  //  override val member: (Elem, Set) => Boolean = {
  //    case (_, E) => false
  //    case (x, T(a, y, b)) =>
  //      if(x < y) member(x, a)
  //      else if(y < x) member(x, b)
  //      else true
  //  }
  override val insert: (Elem, Set) => Set = {
    case (x, E) => T(E, x, E)
    case (x, s@T(a, y, b)) =>
      if (x < y) T(insert(x, a), y, b)
      else if (y < x) T(a, y, insert(x, b))
      else s
  }
}