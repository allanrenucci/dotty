class Foo {
  val a: Any = 3
  a match {
    case 1 =>
    case 2 =>
    case 3 =>
    case _ =>
  }
}

class Bar[T] {
  val a: T = ???
  a match {
    case 1 =>
    case 2 =>
    case 3 =>
    case _ =>
  }
}

class Baz {
  val a: Double = 1.0
  a match {
    case 1 =>
    case 2 =>
    case 3 =>
    case _ =>
  }
}
