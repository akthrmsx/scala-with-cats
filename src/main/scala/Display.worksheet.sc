trait Display[A]:
  def display(value: A): String

object Display:
  given Display[String] with
    def display(value: String): String =
      value

  given Display[Int] with
    def display(value: Int): String =
      value.toString()

  def display[A](value: A)(using d: Display[A]): String =
    d.display(value)

  def print[A](value: A)(using Display[A]) =
    println(display(value))
