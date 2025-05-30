trait Display[A]:
  def display(value: A): String

object Display:
  given Display[String] with
    def display(value: String): String =
      value

  given Display[Int] with
    def display(value: Int): String =
      value.toString()

  given (using Display[String], Display[Int]): Display[Cat] with
    def display(value: Cat): String =
      s"${Display.display(value.name)} is a ${Display.display(value.age)} year-old ${Display.display(value.color)} cat."

  def display[A](value: A)(using d: Display[A]): String =
    d.display(value)

  def print[A](value: A)(using Display[A]) =
    println(display(value))

final case class Cat(name: String, age: Int, color: String)

object DisplaySyntax:
  extension [A](value: A)(using Display[A])
    def display: String =
      Display.display(value)

    def print =
      Display.print(value)

import DisplaySyntax.*

Cat("A", 1, "black").print
