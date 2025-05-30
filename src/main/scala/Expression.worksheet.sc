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
    this match
      case Lit(value)       => value
      case Add(left, right) => left.eval + right.eval
      case Sub(left, right) => left.eval - right.eval
      case Mul(left, right) => left.eval * right.eval
      case Div(left, right) => left.eval / right.eval

object Expression:
  def apply(value: Double): Expression = Lit(value)

val fortyTwo = ((Expression(15) + Expression(5)) * Expression(2) + Expression(2)) / Expression(1)
fortyTwo.eval
