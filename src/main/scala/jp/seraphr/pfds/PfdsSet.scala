package jp.seraphr.pfds

/**
 */
trait PfdsSet {
  type Elem
  type Set

  val empty: Set
  val insert: (Elem, Set) => Set
  val member: (Elem, Set) => Boolean
}

trait TreeSetBase extends PfdsSet with TreeModule {
  type Set = Tree
}

trait TreeSet extends TreeSetBase {
  override type Elem <: Ordered[Elem]
  override val empty: Set = E

  /** 演習問題2.2  */
  private def memberImpl(aElem: Elem, aSet: Set, aCandidate: Option[Elem]): Boolean = (aElem, aSet) match {
    case (x, E) => aCandidate.fold(false)(_.compare(x) == 0)
    case (x, T(a, y, b)) =>
      if (x <= y) memberImpl(x, a, Some(y))
      else memberImpl(x, b, aCandidate)
  }

  override val member: (Elem, Set) => Boolean = memberImpl(_, _, None)

  case object SameElementException extends RuntimeException

  /** 演習問題2.3  */
  //  private def insertImpl(aElem: Elem, aSet: Set): Set = (aElem, aSet) match {
  //    case (x, E) => T(E, x, E)
  //    case (x, s@T(a, y, b)) =>
  //      if (x < y) T(insertImpl(x, a), y, b)
  //      else if (y < x) T(a, y, insertImpl(x, b))
  //      else throw SameElementException
  //  }

  /** 演習問題2.4  */
  private def insertImpl(aElem: Elem, aSet: Set, aCandidate: Option[Elem]): Set = (aElem, aSet) match {
    case (x, E) if aCandidate.fold(false)(_.compare(x) == 0) => throw SameElementException
    case (x, E)                                              => T(E, x, E)
    case (x, s @ T(a, y, b)) =>
      if (x <= y) T(insertImpl(x, a, Some(y)), y, b)
      else T(a, y, insertImpl(x, b, aCandidate))
  }

  override val insert: (Elem, Set) => Set = (aElem, aSet) => try insertImpl(aElem, aSet, None) catch {
    case SameElementException => aSet
  }

  /** 演習課題2.5 (a) */
  def complete0(aElem: Elem, aDepth: Int): Set = {
    if (aDepth <= 0) E
    else {
      val tTree = complete0(aElem, aDepth - 1)
      T(tTree, aElem, tTree)
    }
  }

  /** 演習課題2.5 (b) */
  private def create2(aElem: Elem, aSize: Int): (Set, Set) = (complete(aElem, aSize), complete(aElem, aSize + 1))
  def complete(aElem: Elem, aSize: Int): Set = {

    if (aSize <= 0) E
    else if (aSize % 2 == 0) {
      val (tLeft, tRight) = create2(aElem, (aSize - 1) / 2)
      T(tLeft, aElem, tRight)
    } else {
      val tTree = complete(aElem, (aSize - 1) / 2)
      T(tTree, aElem, tTree)
    }
  }
}