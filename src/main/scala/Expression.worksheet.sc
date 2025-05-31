import scala.annotation.tailrec

type Continuation = Double => Call

enum Call:
  case Continue(value: Double, k: Continuation)
  case Loop(expr: Expression, k: Continuation)
  case Done(result: Double)

enum Expression:
  case Lit(value: Double)
  case Add(left: Expression, right: Expression)
  case Sub(left: Expression, right: Expression)
  case Mul(left: Expression, right: Expression)
  case Div(left: Expression, right: Expression)

  def +(that: Expression): Expression = Add(this, that)
  def -(that: Expression): Expression = Sub(this, that)
  def *(that: Expression): Expression = Mul(this, that)
  def /(that: Expression): Expression = Div(this, that)

  def eval: Double =
    def loop(expr: Expression, cont: Continuation): Call =
      expr match
        case Lit(value)       => Call.Continue(value, cont)
        case Add(left, right) => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l + r, cont)))
        case Sub(left, right) => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l - r, cont)))
        case Mul(left, right) => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l * r, cont)))
        case Div(left, right) => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l / r, cont)))

    @tailrec
    def trampoline(call: Call): Double =
      call match
        case Call.Continue(value, k) => trampoline(k(value))
        case Call.Loop(expr, k)      => trampoline(loop(expr, k))
        case Call.Done(result)       => result

    trampoline(loop(this, Call.Done(_)))

object Expression:
  def apply(value: Double): Expression = Lit(value)

val fortyTwo = ((Expression(15) + Expression(5)) * Expression(2) + Expression(2)) / Expression(1)
fortyTwo.eval
