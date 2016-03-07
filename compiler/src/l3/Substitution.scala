package l3

import scala.collection.immutable.Map

class Substitution[T] private (subst: Map[T, T]) extends (T => T) {
  def apply(e: T): T =
    if (subst contains e) apply(subst(e)) else e

  def +(pair: (T, T)): Substitution[T] =
    new Substitution(subst + pair)

  def ++(pairs: Seq[(T, T)]): Substitution[T] =
    new Substitution(subst ++ pairs)
}

object Substitution {
  def apply[T](from: T, to: T): Substitution[T] =
    empty + (from -> to)

  def apply[T](from: Seq[T], to: Seq[T]): Substitution[T] = {
    require(from.length == to.length)
    empty ++ (from zip to)
  }

  def empty[T]: Substitution[T] = new Substitution(Map.empty[T, T])
}
