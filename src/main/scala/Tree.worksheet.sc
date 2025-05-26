enum Tree[A]:
  case Leaf(value: A)
  case Node(left: Tree[A], right: Tree[A])

  def fold[B](leaf: A => B)(node: (B, B) => B): B =
    this match
      case Leaf(value)       => leaf(value)
      case Node(left, right) => node(left.fold(leaf)(node), right.fold(leaf)(node))

  def size: Int =
    this.fold(_ => 1)(_ + _)

  def contains(elem: A): Boolean =
    this.fold(_ == elem)(_ || _)

  def map[B](f: A => B): Tree[B] =
    this.fold(value => Leaf(f(value)))((left, right) => Node(left, right))
