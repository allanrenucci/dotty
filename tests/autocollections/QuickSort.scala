import scala.collection.AutoCollections.AutoSeq
import scala.collection.mutable

object QuickSort {

  def apply(seq: mutable.Seq[Int]): Unit = {
    apply(seq, 0, seq.length - 1)
  }

  def apply(seq: mutable.Seq[Int], low: Int, high: Int): Unit = {
    if (low < high) {
      val p = partition(seq, low, high)
      apply(seq, low, p - 1)
      apply(seq, p + 1, high)
    }

  }

  private def partition(seq: mutable.Seq[Int], low: Int, high: Int): Int = {
    def swap(i: Int, j: Int) : Unit = {
      val tmp = seq(i)
      seq(i) = seq(j)
      seq(j) = tmp
    }

    val pivot = seq(high)

    var i = low

    for (j <- low until high) {
      if (seq(j) < pivot) {
        swap(i, j)
        i += 1
      }
    }

    swap(i, high)
    i
  }

  def main(args: Array[String]): Unit = {
    val seq = AutoSeq(4, 3, 1)(AutoSeq.Mutable)
    QuickSort(seq)
    println(seq)
  }
}
