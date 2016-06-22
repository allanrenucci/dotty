import scala.collection.AutoCollections._

object Experiment {
  def main(args: Array[String]): Unit = {
    // ------------ Immutable ------------
    val seq1 = AutoSeq("a", "b") // ListBuffer
    val a = seq1.head
    val b = seq1.tail

    val seq2 = AutoSeq[Double]()(Immutable) // immutable.Queue
    val c = 1.0 +: seq2
    val d = seq2 :+ 1.0

    val seq3 = AutoSeq[Long](1L)(Immutable) // WrappedArray
    val e = seq3(1)
    val f = seq3.isDefinedAt(3)

    val char = 'c'
    val seq4 = AutoSeq('a', 'b', char) // WrappedString
    val g = seq4(1)

    val seq5 = AutoSeq(1, 3, 5, 7) // Range.inclusive(1, 7, 2)

    val map1 = AutoMap(1 -> 2) // imutable.HashMap
    val h = map1 + (3 -> 4)
    val i = map1 ++ Seq(3 -> 4, 2 -> 4)

    val map2 = AutoMap[Long, Int](1L -> 2)(Immutable) // LongMap

    val map3 = AutoMap[String, Int]()(Immutable) // mutable.AnyRefMap

    val set1 = AutoSet[String]("A")(Immutable) // immutable.HashSet


    // ------------ Mutable ------------
    val seq6 = AutoSeq(1)(Mutable) // WrappedArray
    seq6(3) = 2

    val seq7 = AutoSeq("Hello")(Mutable) // mutable.UnrolledBuffer

    val map4 = AutoMap[Int, Int]()(Mutable) // mutable.HashMap

    val set2 = AutoSet[String]("A")(Mutable) // mutable.HashSet

    ()
  }
}
