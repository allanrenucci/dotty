object Test {

  abstract class A {
    class B
    def apply: B = new B
  }

  object C extends A

  def i961(): Unit = {
    val a = C.apply
    val b = C.apply

    // val $1$: C.type = C
    // val a = $1$.apply
    // val b = $1$.apply
  }

  def i972(): Unit = {
    // Math should not be used as object
    val a = Math.sqrt(3)
    val b = Math.sqrt(3)
  }

}