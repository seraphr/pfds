package jp.seraphr.pfds

trait UnbalancedSet extends TreeSetBase {
  self: PfdsOrdered =>

  override type Elem = self.T
  override val empty: Set = E
  override val member: (Elem, Set) => Boolean = {
    case (_, E) => false
    case (x, T(a, y, b)) =>
      if (lt(x, y)) member(x, a)
      else if (lt(y, x)) member(x, b)
      else true
  }

  override val insert: (Elem, Set) => Set = {
    case (x, E) => T(E, x, E)
    case (x, s @ T(a, y, b)) =>
      if (lt(x, y)) T(insert(x, a), y, b)
      else if (lt(y, x)) T(a, y, insert(x, b))
      else s
  }
}
