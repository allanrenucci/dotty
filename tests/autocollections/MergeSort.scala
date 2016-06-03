import scala.collection.AutoCollections.AutoSeq

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
    var i1, i2 = 0

    var result = Seq.empty[Int]

    while (i1 < seq1.length && i2 < seq2.length) {
      if (seq1(i1) < seq2(i2)) {
        result :+= seq1(i1)
        i1 += 1
      } else {
        result :+= seq2(i2)
        i2 += 1
      }
    }

    while (i1 < seq1.length) {
      result :+= seq1(i1)
      i1 += 1
    }

    while (i2 < seq2.length) {
      result :+= seq2(i2)
      i2 += 1
    }

    result
  }

  def main(args: Array[String]): Unit = {
    val seq = AutoSeq(4, 3, 1)
    MergeSort(seq)
  }
}
