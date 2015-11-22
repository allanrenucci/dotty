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
    assert(a.count == 1)
    assert(count == 1)

    a.count = 0
    count = 0
  }

  def test8(): Unit = {
    def impure = 1
    val a = idem4(impure, idem1)
    val b = idem1
    assert(count == 3)

    count = 0
  }

  def test9(): Unit = {
    def impure = new A
    val a = impure.idem3(idem1)
    val b = idem1
    assert(impure.count == 1)
    assert(count == 2)

    count = 0
  }

}

object B {
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
    assert(a.count == 1)
    assert(count == 1)

    a.count = 0
    count = 0
  }

  def test8(): Unit = {
    def impure = 1
    val a = idem4(impure, idem1)
    val b = idem1
    assert(count == 3)

    count = 0
  }

  def test9(): Unit = {
    def impure = new A
    val a = impure.idem3(idem1)
    val b = idem1
    assert(impure.count == 1)
    assert(count == 2)

    count = 0
  }

}

object Test {

  def main(args: Array[String]) = {
    import B._

    val i = new A
    val i2 = new A

    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
    test7(i2)

    i.test1()
    i.test2()
    i.test3()
    i.test4()
    i.test5()
    i.test6()
    i.test7(i2)
  }
}