enum Regexp:
  case Append(left: Regexp, right: Regexp)
  case OrElse(first: Regexp, second: Regexp)
  case Repeat(source: Regexp)
  case Apply(string: String)
  case Empty

  def ++(that: Regexp): Regexp = Append(this, that)

  def orElse(that: Regexp): Regexp = OrElse(this, that)

  def repeat: Regexp = Repeat(this)

  def `*` : Regexp = this.repeat

  def matches(input: String): Boolean =
    def loop(regexp: Regexp, idx: Int): Option[Int] =
      regexp match
        case Append(left, right)   => loop(left, idx).flatMap(idx => loop(right, idx))
        case OrElse(first, second) => loop(first, idx).orElse(loop(second, idx))
        case Repeat(source)        => loop(source, idx).flatMap(idx => loop(regexp, idx)).orElse(Some(idx))
        case Apply(string)         => Option.when(input.startsWith(string, idx))(idx + string.size)
        case Empty                 => None

    loop(this, 0).map(idx => idx == input.size).getOrElse(false)

object Regexp:
  val empty: Regexp = Empty

  def apply(string: String): Regexp = Apply(string)

val regexp = Regexp("Sca") ++ Regexp("la") ++ Regexp("la").repeat

regexp.matches("Scala")
regexp.matches("Scalala")

regexp.matches("Sca")
regexp.matches("Scaland")
