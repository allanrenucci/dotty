package scala.collection

object AutoCollections {

  class AutoSeq[T] extends Seq[T] {
    override def length: Int           = ???
    override def apply(idx: Int): T    = ???
    override def iterator: Iterator[T] = ???
  }

  object AutoSeq {
    def apply[T](xs: T*): Seq[T] = ???
  }

  class AutoSeqInt extends Seq[Int] {
    override def length: Int             = ???
    override def apply(idx: Int): Int    = ???
    override def iterator: Iterator[Int] = ???
  }

  object AutoSeqInt {
    def apply(xs: Int*): Seq[Int] = ???
  }

  class AutoMap[A, B] extends Map[A, B] {
    override def get(key: A): Option[B]              = ???
    override def +[B1 >: B](kv: (A, B1)): Map[A, B1] = ???
    override def iterator: scala.Iterator[(A, B)]    = ???
    override def -(key: A): Map[A, B]                = ???
  }

  object AutoMap {
    def apply[A, B](xs: (A, B)*): Map[A, B] = ???
  }

  class AutoSet[T] extends Set[T] {
    override def contains(elem: T): Boolean  = ???
    override def +(elem: T): Set[T]          = ???
    override def -(elem: T): Set[T]          = ???
    override def iterator: scala.Iterator[T] = ???
  }

  object AutoSet {
    def apply[T](xs: T*): Set[T] = ???
  }

}
