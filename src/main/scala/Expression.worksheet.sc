type Continuation = Double => Double

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
    def loop(expr: Expression, cont: Continuation): Double =
      expr match
        case Lit(value)       => cont(value)
        case Add(left, right) => loop(left, l => loop(right, r => cont(l + r)))
        case Sub(left, right) => loop(left, l => loop(right, r => cont(l - r)))
        case Mul(left, right) => loop(left, l => loop(right, r => cont(l * r)))
        case Div(left, right) => loop(left, l => loop(right, r => cont(l / r)))

    loop(this, identity)

object Expression:
  def apply(value: Double): Expression = Lit(value)

val fortyTwo = ((Expression(15) + Expression(5)) * Expression(2) + Expression(2)) / Expression(1)
fortyTwo.eval
