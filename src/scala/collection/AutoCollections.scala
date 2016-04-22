package scala.collection

import scala.collection.immutable.WrappedString
import scala.reflect.ClassTag

object AutoCollections {

  // ------------ Seq ------------

  object AutoSeq {
    //def apply[T](xs: T*): Seq[T] = ???

    sealed trait Semantic { type Ret[T] }
    object Immutable extends Semantic { type Ret[T] = Seq[T] }
    object Mutable   extends Semantic { type Ret[T] = mutable.Seq[T] }
    object Lazy      extends Semantic { type Ret[T] = Iterable[T] }

    implicit def defaultSemantic: Immutable.type = Immutable

    def apply[T](xs: T*)(implicit sem: Semantic): sem.Ret[T] = ???
  }

  class ImmutableSeq[T] extends Seq[T] {
    override def length: Int           = ???
    override def apply(idx: Int): T    = ???
    override def iterator: Iterator[T] = ???
  }

  class MutableSeq[T] extends mutable.Seq[T] {
    override def update(idx: Int, elem: T): Unit = ???
    override def length: Int                     = ???
    override def apply(idx: Int): T              = ???
    override def iterator: Iterator[T]           = ???
  }

  class LazySeq[T] extends Iterable[T] {
    override def iterator: Iterator[T] = ???
  }


  // ------------ Map ------------

  object AutoMap {
    //def apply[A, B](xs: (A, B)*): Map[A, B] = ???

    sealed trait Semantic { type Ret[A, B] }
    object Immutable extends Semantic { type Ret[A, B] = Map[A, B] }
    object Mutable   extends Semantic { type Ret[A, B] = mutable.Map[A, B] }
    object Lazy      extends Semantic { type Ret[A, B] = Iterable[(A, B)] }

    implicit def defaultSemantic: Immutable.type = Immutable

    def apply[A, B](xs: (A, B)*)(implicit sem: Semantic): sem.Ret[A, B] = ???
  }

  class ImmutableMap[A, B] extends Map[A, B] {
    override def get(key: A): Option[B]              = ???
    override def +[B1 >: B](kv: (A, B1)): Map[A, B1] = ???
    override def iterator: Iterator[(A, B)]          = ???
    override def -(key: A): Map[A, B]                = ???
  }

  class MutableMap[A, B] extends mutable.Map[A, B] {
    override def +=(kv: (A, B)): MutableMap.this.type = ???
    override def -=(key: A): MutableMap.this.type     = ???
    override def get(key: A): Option[B]               = ???
    override def iterator: Iterator[(A, B)]           = ???
  }

  class LazyMap[A, B] extends Iterable[(A, B)] {
    override def iterator: Iterator[(A, B)] = ???
  }


  // ------------ Set ------------

  object AutoSet {
    //def apply[T](xs: T*): Set[T] = ???

    sealed trait Semantic { type Ret[T] }
    object Immutable extends Semantic { type Ret[T] = Set[T] }
    object Mutable   extends Semantic { type Ret[T] = mutable.Set[T] }
    object Lazy      extends Semantic { type Ret[T] = Iterable[T] }

    implicit def defaultSemantic: Immutable.type = Immutable

    def apply[T](xs: T*)(implicit sem: Semantic): sem.Ret[T] = ???
  }

  class ImmutableSet[T] extends Set[T] {
    override def contains(elem: T): Boolean  = ???
    override def +(elem: T): Set[T]          = ???
    override def -(elem: T): Set[T]          = ???
    override def iterator: scala.Iterator[T] = ???
  }

  class MutableSet[T] extends mutable.Set[T] {
    override def +=(elem: T): MutableSet.this.type = ???
    override def -=(elem: T): MutableSet.this.type = ???
    override def contains(elem: T): Boolean        = ???
    override def iterator: Iterator[T]             = ???
  }

  class LazySet[T] extends Iterable[T] {
    override def iterator: Iterator[T] = ???
  }

  def wrappedString(chars: Char*): WrappedString =
    new WrappedString(chars.mkString)

  def wrappedArray[T](args: T*)(implicit classTag: ClassTag[T]): mutable.WrappedArray[T] =
    mutable.WrappedArray.make[T](Array(args: _*)(classTag))
}
