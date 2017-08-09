package jp.seraphr.pfds

import scala.language.higherKinds

/**
 */
trait FiniteMap {
  type Key
  type Map[A]

  def empty[A]: Map[A]
  def bind[A](k: Key, v: A, m: Map[A]): Map[A]
  def lookup[A](k: Key, m: Map[A]): A

  case class NotFoundException(aKey: Key) extends RuntimeException
}

/** 演習課題2.6 */
trait TreeFiniteMap extends FiniteMap {
  self: PfdsOrdered =>

  import ParameterizedTreeModule._

  override type Key = self.T
  override type Map[A] = Tree[(Key, A)]

  override def empty[A]: Map[A] = E
  override def lookup[A](k: Key, m: Map[A]): A = m match {
    case E => throw NotFoundException(k)
    case T(a, (k2, v), b) =>
      if (lt(k, k2)) lookup(k, a)
      else if (lt(k2, k)) lookup(k, b)
      else v
  }

  override def bind[A](k: Key, v: A, m: Map[A]): Map[A] = m match {
    case E => T(empty, (k, v), empty)
    case T(a, (k2, _), b) =>
      if (lt(k, k2)) bind(k, v, a)
      else if (lt(k2, k)) bind(k, v, b)
      else T(a, (k, v), b)
  }
}