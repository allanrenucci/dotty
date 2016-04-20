import scala.collection.AutoCollections._

object Experiment {
  def main(args: Array[String]): Unit = {

    // ------------ Immutable ------------

    val seq1 = AutoSeq("a", "b") // ListBuffer
    val a = seq1.head
    val b = seq1.tail

    val seq2 = AutoSeq[Int]()(AutoSeq.Immutable) // immutable.Queue
    val c = 1 +: seq2
    val d = seq2 :+ 1

//    val seq3 = AutoSeq[Long](1L)(AutoSeq.Immutable) // Array
//    val e = seq3(1)
//    val f = seq3.isDefinedAt(3)

    val seq4 = AutoSeq[Char]() // Vector

    val map1 = AutoMap(1 -> 2) // imutable.HashMap
    val h = map1 + (3 -> 4)
    val i = map1 ++ Seq(3 -> 4, 2, 4)

    val map2 = AutoMap[Long, Int](1L -> 2)(AutoMap.Immutable) // LongMap

    val map3 = AutoMap[String, Int]()(AutoMap.Immutable) // mutable.HashMap

    val set1 = AutoSet[String]("A")(AutoSet.Immutable) // immutable.HashSet


    // ------------ Mutable ------------
//    val seq5 = AutoSeq(1)(AutoSeq.Mutable) // Array
//    seq5(3) = 2

    val seq6 = AutoSeq("Hello")(AutoSeq.Mutable) // ListBuffer

    val map4 = AutoMap[Int, Int]()(AutoMap.Mutable) // mutable.HashMap

    val set2 = AutoSet[String]("A")(AutoSet.Mutable) // mutable.HashSet

    ()
  }
}