trait Set[A]:
  def contains(elem: A): Boolean
  def insert(elem: A): Set[A]

  def union(that: Set[A]): Set[A] =
    val self = this
    new Set[A]:
      def contains(elem: A): Boolean =
        self.contains(elem) || that.contains(elem)

      def insert(elem: A): Set[A] =
        self.insert(elem).union(that)

  def containsAll(elems: Iterable[A]): Boolean =
    elems.forall(elem => this.contains(elem))

final class ListSet[A](elems: List[A]) extends Set[A]:
  override def contains(elem: A): Boolean =
    elems.contains(elem)

  override def insert(elem: A): Set[A] =
    ListSet(elem :: elems)
