package jp.seraphr.pfds

/**
 */
trait TreeModule {
  type Elem
  sealed trait Tree
  case object E extends Tree
  case class T(l: Tree, e: Elem, r: Tree) extends Tree
}

object ParameterizedTreeModule {
  sealed trait Tree[+E]
  case object E extends Tree[Nothing]
  case class T[E](l: Tree[E], e: E, r: Tree[E]) extends Tree[E]
}