package scala.collection

import scala.collection.immutable.WrappedString
import scala.reflect.ClassTag

object AutoCollections {

  sealed trait Semantic {
    type Seq[T]
    type Map[K, V]
    type Set[T]
  }

  object Immutable extends Semantic {
    type Seq[T]    = collection.Seq[T]

    //type Map[K, V] = immutable.Map[K, V]
    //type Set[T]    = immutable.Set[T]

    // This is convenient as we can use mutable collections in place of
    // mutable ones but the return type of our API doesn't conform to
    // the Map and Set from Predef (i.e. immutable.Map and immutable.Set)
    type Map[K, V] = collection.Map[K, V]
    type Set[T]    = collection.Set[T]
  }

  object Mutable extends Semantic {
    type Seq[T]    = mutable.Seq[T]
    type Map[K, V] = mutable.Map[K, V]
    type Set[T]    = mutable.Set[T]
  }

  object Lazy extends Semantic {
    type Seq[T]    = Iterable[T]
    type Map[K, V] = Iterable[(K, V)]
    type Set[T]    = Iterable[T]
  }

  implicit def defaultSemantic: Immutable.type = Immutable


  // ------------ Seq ------------

  object AutoSeq {
    def apply[T](xs: T*)(implicit sem: Semantic): sem.Seq[T] = ???
  }

  class ImmutableSeq[T] extends Immutable.Seq[T] {
    override def length: Int           = ???
    override def apply(idx: Int): T    = ???
    override def iterator: Iterator[T] = ???
  }

  class MutableSeq[T] extends Mutable.Seq[T] {
    override def update(idx: Int, elem: T): Unit = ???
    override def length: Int                     = ???
    override def apply(idx: Int): T              = ???
    override def iterator: Iterator[T]           = ???
  }

  class LazySeq[T] extends Lazy.Seq[T] {
    override def iterator: Iterator[T] = ???
  }


  // ------------ Map ------------

  object AutoMap {
    def apply[A, B](xs: (A, B)*)(implicit sem: Semantic): sem.Map[A, B] = ???
  }

  class ImmutableMap[A, B] extends Immutable.Map[A, B] {
    override def +[B1 >: B](kv: (A, B1)): Map[A, B1] = ???
    override def get(key: A): Option[B]              = ???
    override def iterator: Iterator[(A, B)]          = ???
    override def -(key: A): Map[A, B]                = ???
  }

//  class ImmutableMap[A, B] extends Immutable.Map[A, B] {
//    override def +[B1 >: B](kv: (A, B1)): immutable.Map[A, B1] = ???
//    override def get(key: A): Option[B]                        = ???
//    override def iterator: Iterator[(A, B)]                    = ???
//    override def -(key: A): immutable.Map[A, B]                = ???
//  }

  class MutableMap[A, B] extends Mutable.Map[A, B] {
    override def +=(kv: (A, B)): MutableMap.this.type = ???
    override def -=(key: A): MutableMap.this.type     = ???
    override def get(key: A): Option[B]               = ???
    override def iterator: Iterator[(A, B)]           = ???
  }

  class LazyMap[A, B] extends Lazy.Map[A, B] {
    override def iterator: Iterator[(A, B)] = ???
  }


  // ------------ Set ------------

  object AutoSet {
    def apply[T](xs: T*)(implicit sem: Semantic): sem.Set[T] = ???
  }

//  class ImmutableSet[T] extends Immutable.Set[T] {
//    override def contains(elem: T): Boolean   = ???
//    override def +(elem: T): immutable.Set[T] = ???
//    override def -(elem: T): immutable.Set[T] = ???
//    override def iterator: scala.Iterator[T]  = ???
//  }

  class ImmutableSet[T] extends Immutable.Set[T] {
    override def contains(elem: T): Boolean = ???
    override def +(elem: T): Set[T]         = ???
    override def -(elem: T): Set[T]         = ???
    override def iterator: Iterator[T]      = ???
  }

  class MutableSet[T] extends Mutable.Set[T] {
    override def +=(elem: T): MutableSet.this.type = ???
    override def -=(elem: T): MutableSet.this.type = ???
    override def contains(elem: T): Boolean        = ???
    override def iterator: Iterator[T]             = ???
  }

  class LazySet[T] extends Lazy.Set[T] {
    override def iterator: Iterator[T] = ???
  }


  // ------- Helper functions -------

  def wrappedString(chars: Char*): WrappedString =
    new WrappedString(chars.mkString)

  def wrappedArray[T](args: T*)(implicit classTag: ClassTag[T]): mutable.WrappedArray[T] =
    mutable.WrappedArray.make[T](Array(args: _*)(classTag))
}
