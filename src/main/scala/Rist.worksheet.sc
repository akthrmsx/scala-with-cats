enum Rist[A]:
  case Empty()
  case Pair(_head: A, _tail: Rist[A])

  def isEmpty: Boolean =
    this match
      case Empty()    => true
      case Pair(_, _) => false

  def head: Option[A] =
    this match
      case Empty()       => None
      case Pair(head, _) => Some(head)

  def tail: Option[Rist[A]] =
    this match
      case Empty()       => None
      case Pair(_, tail) => Some(tail)

  def map[B](f: A => B): Rist[B] =
    Rist.unfold(this)(_.isEmpty, pair => f(pair.head.get), pair => pair.tail.get)

object Rist:
  def unfold[A, B](seed: A)(stop: A => Boolean, f: A => B, next: A => A): Rist[B] =
    if stop(seed) then Empty()
    else Pair(f(seed), unfold(next(seed))(stop, f, next))

  def fill[A](n: Int)(elem: => A): Rist[A] =
    unfold(n)(_ == 0, _ => elem, _ - 1)

  def iterate[A](start: A, len: Int)(f: A => A): Rist[A] =
    unfold((len, start))((len, _) => len == 0, (_, start) => start, (len, start) => (len - 1, f(start)))
