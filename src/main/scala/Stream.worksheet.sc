trait Stream[A]:
  def head: A
  def tail: Stream[A]

  def take(n: Int): List[A] =
    if n == 0 then Nil else head :: tail.take(n - 1)

  def map[B](f: A => B): Stream[B] =
    val self = this
    new Stream[B]:
      def head: B =
        f(self.head)

      def tail: Stream[B] =
        self.tail.map(f)

  def filter(f: A => Boolean): Stream[A] =
    val self = this
    new Stream[A]:
      def head: A =
        def loop(stream: Stream[A]): A =
          if f(stream.head) then stream.head else loop(stream.tail)

        loop(self)

      def tail: Stream[A] =
        def loop(stream: Stream[A]): Stream[A] =
          if f(stream.head) then stream.tail.filter(f) else loop(stream.tail)

        loop(self)

  def zip[B](that: Stream[B]): Stream[(A, B)] =
    val self = this
    new Stream[(A, B)]:
      def head: (A, B) =
        (self.head, that.head)

      def tail: Stream[(A, B)] =
        self.tail.zip(that.tail)

  def scanLeft[B](zero: B)(f: (B, A) => B): Stream[B] =
    val self = this
    new Stream[B]:
      def head: B =
        zero

      def tail: Stream[B] =
        self.tail.scanLeft(f(zero, self.head))(f)

object Stream:
  val ones: Stream[Int] = new Stream[Int]:
    def head: Int =
      1

    def tail: Stream[Int] =
      ones

  def unfold[A, B](seed: A, f: A => B, next: A => A): Stream[B] =
    new Stream[B]:
      def head: B =
        f(seed)

      def tail: Stream[B] =
        unfold(next(seed), f, next)

Stream.ones.head
Stream.ones.tail.head
Stream.ones.tail.tail.head
Stream.ones.take(5)

val alternating = Stream.unfold(true, if _ then 1 else -1, !_)
alternating.take(5)

val naturals = Stream.ones.scanLeft(0)((b, a) => b + a)
naturals.take(5)
