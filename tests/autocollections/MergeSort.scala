import scala.collection.AutoCollections._

object MergeSort {
  def apply(seq: Seq[Int]): Seq[Int] = {
    if (seq.length <= 1)
      return seq

    val (left, right) = seq.splitAt(seq.length / 2)
    val leftSorted  = apply(left)
    val rightSorted = apply(right)

    merge(leftSorted, rightSorted)
  }

  private def merge(seq1: Seq[Int], seq2: Seq[Int]): Seq[Int] = {
    var result = AutoSeq[Int]()
    var left   = seq1
    var right  = seq2

    while (left.nonEmpty && right.nonEmpty) {
      if (left.head < right.head) {
        result = result :+ left.head
        left = left.tail
      } else {
        result = result :+ right.head
        right = right.tail
      }
    }

    left.foreach { e =>
      result = result :+ e
    }

    right.foreach { e =>
      result = result :+ e
    }

    result
  }

  def main(args: Array[String]): Unit = {
    val seq = AutoSeq(4, 3, 1)
    MergeSort(seq)
  }
}
