trait Set[A]:
  def contains(elem: A): Boolean

  def insert(elem: A): Set[A] =
    InsertOneSet(elem, this)

  def union(that: Set[A]): Set[A] =
    UnionSet(this, that)

  def containsAll(elems: Iterable[A]): Boolean =
    elems.forall(elem => this.contains(elem))

final class ListSet[A](elems: List[A]) extends Set[A]:
  override def contains(elem: A): Boolean =
    elems.contains(elem)

object ListSet:
  def empty[A]: Set[A] =
    ListSet(List.empty)

final class InsertOneSet[A](value: A, src: Set[A]) extends Set[A]:
  override def contains(elem: A): Boolean =
    value == elem || src.contains(elem)

final class UnionSet[A](first: Set[A], second: Set[A]) extends Set[A]:
  override def contains(elem: A): Boolean =
    first.contains(elem) || second.contains(elem)

object Evens extends Set[Int]:
  override def contains(elem: Int): Boolean =
    elem % 2 == 0

val evensAndOne = Evens.insert(1)
val evensAndOthers = Evens.union(ListSet.empty.insert(1).insert(3))

evensAndOne.contains(1)
evensAndOthers.contains(1)

evensAndOne.contains(2)
evensAndOthers.contains(2)

evensAndOne.contains(3)
evensAndOthers.contains(3)

final class IndicatorSet[A](indicator: A => Boolean) extends Set[A]:
  override def contains(elem: A): Boolean =
    indicator(elem)

val odds = IndicatorSet[Int](_ % 2 == 1)

odds.contains(1)
odds.contains(2)
odds.contains(3)

val integers = Evens.union(odds)

integers.contains(1)
integers.contains(2)
integers.contains(3)
