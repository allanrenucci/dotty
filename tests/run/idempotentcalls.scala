import scala.annotation.{Idempotent => idem}

class A {
  var count = 0

  @idem def idem1: Int = {
    count += 1
    count
  }

  @idem def idem2(): Int = {
    count += 1
    count
  }

  @idem def idem3(a: Int): Int = {
    count += 1
    count
  }

  @idem def idem4(a: Int, b: Int): Int = {
    count += 1
    count
  }

  def test1(): Unit = {
    val a = idem1
    val b = idem1

    assert(count == 1)
    count = 0
  }

  def test2(): Unit = {
    val a = idem2()
    val b = idem2()

    assert(count == 1)
    count = 0
  }

  def test3(): Unit = {
    val a = idem2()
    val b = idem2()

    assert(count == 1)
    count = 0
  }

  def test4(): Unit = {
    val a = idem1 + idem2()
    val b = idem1 + idem2()

    assert(count == 2)
    count = 0
  }

  def test5(): Unit = {
    val a = idem1
    (1 to 10) map (_ => idem1)

    assert(count == 1)
    count = 0
  }

  def test6(): Unit = {
    val a = idem3(idem1)
    val b = idem1
    val c = idem3(idem1)

    assert(count == 2)
    count = 0
  }

  def test7(a: A): Unit = {
    val i = a.idem1
    val i2 = idem1
    val i3 = this.idem1

    assert(a.count == 1, s"${a.count} != 1")
    assert(count == 1)
    a.count = 0
    count = 0
  }

  def test8(): Unit = {
    val a = idem4(idem1, idem2())
    val b = idem4(idem1, idem2())

    assert(count == 3)
    count = 0
  }

  def test9(): Unit = {
    val a = idem4(idem1 + 1, idem2())
    val b = idem2()
    val c = idem1

    assert(count == 3)
    count = 0
  }

  def test10(): Unit = {
    var a = idem1
    val b = idem4(a, idem1)
    a = 3
    val c = idem4(a, idem1)

    assert(count == 3)
    count = 0
  }

  def test11(): Unit = {
    def impure = 1

    val a = idem4(impure, idem1)
    val b = idem1

    assert(count == 3)
    count = 0
  }

  def test12(): Unit = {
    def impure = new A

    val a = impure.idem3(idem1)
    val c = idem3(idem1)

    assert(count == 3)
    count = 0
  }

  def test13(): Unit = {
    def impure = 3

    val a = impure + idem1
    val b = impure + idem1

    assert(count == 2)
    count = 0
  }

  def test14(): Unit = {
    def impure = 3

    val a = idem1 + impure
    val b = impure + idem1

    assert(count == 1)
    count = 0
  }

  def test15(): Unit = {
    def impure = 5
    val a = idem4(3 + impure, idem1)
    val b = idem1

    assert(count == 3)
    count = 0
  }

  def test16(): Unit = {
    def impure = 5
    val a = idem4(idem1, impure)
    val b = idem1

    assert(count == 2)
    count = 0
  }

  def test17(): Unit = {
    def impure = 5
    val a = idem4(idem1 + impure, idem2())
    val b = idem1
    val c = idem2()

    assert(count == 4)
    count = 0
  }

  def test18(): Unit = {
    val a = idem3(1)
    val b = idem3(1)
    val c = idem4(2, 3)
    val d = idem4(2, 3)

    assert(count == 2)
    count = 0
  }

  def test19(): Unit = {
    val a = idem3(1)
    val b = idem3(2)
    val c = idem4(1, 3)
    val d = idem4(2, 3)

    assert(count == 4)
    count = 0
  }

  def test20(): Unit = {
    val a: Any = idem1
    val b = idem1

    assert(count == 1)
    count = 0
  }

  def test21(): Unit = {
    @idem def foo(i: A, j: Int) = {
      count += 1
      j
    }

    val a = foo(this, 1)
    val b = foo(this, 1)
    assert(count == 1)
    count = 0
  }

  def test22(cond: Boolean): Unit = {
    val a = idem1

    if (cond) idem1 else idem1

    assert(count == 1)
    count = 0
  }

  def test23(cond: Boolean): Unit = {
    if (cond) idem1 else idem1
    val a = idem1

    assert(count == 2)
    count = 0
  }

  def test24(any: Any): Unit = {
    val a = idem1

//    any match {
//      case _: String => idem1
//      case _ => idem1
//    }

    assert(count == 1)
    count = 0
  }

  def test25(): Unit = {
    @idem def idem = {
      count += 1
      true
    }

    if (idem) 1 else 2
    val a = idem

    assert(count == 1)
    count = 0
  }
}

object Test {

  def main(args: Array[String]) = {

    val a = new A
    import a._

    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
    test7(new A)
    test8()
    test9()
    test10()
    test11()
    test12()
    test13()
    test14()
    test15()
    test16()
//    test17()
    test18()
    test19()
    test20()
    test22(true)
    test22(false)
    test23(true)
    test23(false)
    test24("Hello")
    test24(1)
    test25()
  }
}