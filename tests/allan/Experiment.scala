import scala.collection.AutoCollections._

object Experiment {
  def main(args: Array[String]): Unit = {

//    val seq1 = AutoSeq(1, 2)
//    val seq2 = AutoSeq(1, 2)(AutoSeq.Mutable)
//
//    val map2 = AutoMap(1 -> 2, 3 -> 4)
//    val map1 = AutoMap(1 -> 2, 3 -> 4)(AutoMap.Mutable)
//
//    val set1 = AutoSet(1, 2)
//    val set2 = AutoSet(1, 2)(AutoSet.Mutable)

    val seq1 = AutoSeq[Int]()
    seq1.head
    seq1.tail

    val seq2 = AutoSeq[String]()
    1 +: seq2
    seq2 :+ 1

    val seq3 = AutoSeq[Char]()
    seq3(5)

    val seq4 = AutoSeq[Long]()
    seq4.iterator

//    val map = AutoMap[Int, String]()
//    val c = map.get(3)
//    val d = map.iterator

    ()
  }
}