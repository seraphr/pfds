package jp.seraphr.pfds

/**
 */
trait PfdsOrdered {
  type T
  def eq(a: T, b: T): Boolean
  def lt(a: T, b: T): Boolean
  def leq(a: T, b: T): Boolean
}
