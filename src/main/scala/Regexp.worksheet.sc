type Continuation = Option[Int] => Call

enum Call:
  case Continue(idx: Option[Int], k: Continuation)
  case Loop(regexp: Regexp, idx: Int, k: Continuation)
  case Done(result: Option[Int])

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
    def loop(regexp: Regexp, idx: Int, cont: Continuation): Call =
      regexp match
        case Append(left, right) =>
          val k: Continuation =
            case Some(idx) => Call.Loop(right, idx, cont)
            case None      => Call.Continue(None, cont)
          Call.Loop(left, idx, k)
        case OrElse(first, second) =>
          val k: Continuation =
            case idx @ Some(_) => Call.Continue(idx, cont)
            case None          => Call.Loop(second, idx, cont)
          Call.Loop(first, idx, k)
        case Repeat(source) =>
          val k: Continuation =
            case Some(idx) => Call.Loop(regexp, idx, cont)
            case None      => Call.Continue(Some(idx), cont)
          Call.Loop(source, idx, k)
        case Apply(string) => Call.Continue(Option.when(input.startsWith(string, idx))(idx + string.size), cont)
        case Empty         => Call.Continue(None, cont)

    def trampoline(call: Call): Option[Int] =
      call match
        case Call.Continue(idx, k)     => trampoline(k(idx))
        case Call.Loop(regexp, idx, k) => trampoline(loop(regexp, idx, k))
        case Call.Done(result)         => result

    trampoline(loop(this, 0, Call.Done(_))).map(idx => idx == input.size).getOrElse(false)

object Regexp:
  val empty: Regexp = Empty

  def apply(string: String): Regexp = Apply(string)

val regexp = Regexp("Sca") ++ Regexp("la") ++ Regexp("la").repeat

regexp.matches("Scala")
regexp.matches("Scalala")

regexp.matches("Sca")
regexp.matches("Scaland")
