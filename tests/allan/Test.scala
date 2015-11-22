import scala.annotation.Idempotent
import scala.tools.nsc.backend.icode.Primitives

class Test {

  lazy val idem1 = "Hello"

  @Idempotent def idem2 = "Hello"

  @Idempotent def idem3(a: String) = "Hello"

  @Idempotent def idem4(a: String, b: String) = "Hello"

  def test1(): Unit = {
    lazy val bar = "Hello"
    val a = bar
    val b = bar

    //    lazy val bar = "Hello"
    //    val a = bar
    //    val b = $1$
  }

  def test2(): Unit = {
    val a = idem1
    val b = idem1

    //     val $1$ = idem1
    //     val a = $1$
    //     val b = $1$
  }

  def test3(): Unit = {
    val a = idem2
    val b = idem2

    //     val $1$ = idem2
    //     val a = $1$
    //     val b = $1$
  }

  def test5(): Unit = {
    lazy val bar = "Hello"
    val a = bar + idem1
    val b = bar + idem1
  }

  def test6(): Unit = {
    lazy val bar = "Hello"
    val a = bar + 1
    val b = bar
  }

  def test7(): Unit = {
    lazy val bar = "Hello"
    val a = 1 + bar
    val b = bar
  }

  def test8(): Unit = {
    val a = idem4(idem1, idem2)
    val b = idem4(idem1, idem2)
  }

  def test9(): Unit = {
    val a = idem4(idem1 + 1, idem2)
    val b = idem2
  }

  def test10(): Unit = {
    var a = idem1
    val b = idem4(a, idem1)
    a = "allan"
    val c = idem3(a)
    val d = idem1
  }

  def test11(): Unit = {
    def impure = "impure"
    val a = idem4(impure, idem1)
    val b = idem1
  }

  def test12(): Unit = {
    def impure = new Test
    val a = impure.idem3(idem1)
    val b = idem1
  }

  def test13(): Unit = {
    def impure = " allan"
    val a = idem4("Hello" + impure, idem1)
    val b = idem1

  }

  def test14(): Unit = {
    val a = idem1
    List(1, 2, 3) map (_ => idem1)
  }

  def test15(): Unit = {
    val a = idem3("Hello")
    val b = idem3("Hello")
    val c = idem4("Allan", "Hello")
    val d = idem4("Allan", "Hello")
  }

  def test16(): Unit = {
    val a: AnyRef = idem1
    val b = idem1
  }

}
