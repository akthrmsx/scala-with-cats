trait Bool:
  def `if`[A](t: A)(f: A): A

object Bool:
  val True =
    new Bool:
      def `if`[A](t: A)(f: A): A =
        t

  val False =
    new Bool:
      def `if`[A](t: A)(f: A): A =
        f

  def and(left: Bool, right: Bool): Bool =
    new Bool:
      def `if`[A](t: A)(f: A): A =
        left.`if`(right)(False).`if`(t)(f)

  def or(left: Bool, right: Bool): Bool =
    new Bool:
      def `if`[A](t: A)(f: A): A =
        left.`if`(True)(right).`if`(t)(f)

  def not(b: Bool): Bool =
    new Bool:
      def `if`[A](t: A)(f: A): A =
        b.`if`(False)(True).`if`(t)(f)

Bool.or(Bool.True, Bool.True).`if`("YES")("NO")
Bool.or(Bool.True, Bool.False).`if`("YES")("NO")
Bool.or(Bool.False, Bool.True).`if`("YES")("NO")
Bool.or(Bool.False, Bool.False).`if`("YES")("NO")

Bool.not(Bool.True).`if`("YES")("NO")
Bool.not(Bool.False).`if`("YES")("NO")
