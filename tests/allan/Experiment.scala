import scala.collection.AutoCollections._

object Experiment {
  def main(args: Array[String]) = {
    //val seq = AutoSeq[Int](1, 2) // fails with Ycheck
    val seq = AutoSeq[Int]()
    val a = seq(10)
    val b = seq.iterator

    val map = AutoMap[Int, String]()
    val c = map.get(3)
    val d = map.iterator

    val set = AutoSet[List[String]]()
    val e = set.contains(Nil)
    val f = set.iterator
  }
}