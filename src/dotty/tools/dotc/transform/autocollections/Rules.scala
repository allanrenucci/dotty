package dotty.tools.dotc.transform.autocollections

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.autocollections.AutoCollections._

object Rules {

  import tpd._

  /** Evaluate a list of rules and return the tree associated with the first satisfied rule */
  def evaluate[T <: AutoCollection](rules: List[Rule[T]], collection: T, methods: Set[Name])
                                   (implicit ctx: Context): Option[Tree] = rules match {
    case rule :: rs =>
      if (rule.isSatisfied(collection, methods))
        Some(rule.tree(collection))
      else
        evaluate(rs, collection, methods)
    case Nil =>
      None

  }

  /** A rule define a specific collection implementation and under which
    * conditions this implementation is optimal
    */
  trait Rule[T <: AutoCollection] {

    /** Collection implementation associated to this rule */
    def path: String

    /** Test whether this rule is satisfied for `collection` */
    def isSatisfied(collection: T, methods: Set[Name])(implicit ctx: Context): Boolean

    /** Create an instance of the collection associated to this rule */
    def tree(collection: T)(implicit ctx: Context): Tree = {
      // Default to Module.apply[tpParams](elems)
      ref(module)
        .select(nme.apply)
        .appliedToTypes(collection.tpParams)
        .appliedToArgs(collection.elems)
    }

    protected def module(implicit ctx: Context): Symbol = ctx.requiredModule(path)
  }

  // ------------ Seq ------------

  class ListR extends Rule[AutoSeq] {

    def path: String = "scala.collection.immutable.List"

    def isSatisfied(seq: AutoSeq, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      seq.semantic == Immutable && methods.containsOnly(
        head, tail, prepend, nonEmpty, isEmpty
      )
    }
  }

  class Queue extends Rule[AutoSeq] {

    def path = "scala.collection.immutable.Queue"

    def isSatisfied(seq: AutoSeq, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      seq.semantic == Immutable && methods.contains(append)
    }
  }

  class WrappedString extends Rule[AutoSeq] {

    def path: String = "scala.collection.immutable.WrappedString"

    def isSatisfied(seq: AutoSeq, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      seq.semantic == Immutable &&
      methods.containsSome(apply, isDefinedAt) &&
      seq.tpParam =:= ctx.definitions.CharType
    }

    override def tree(seq: AutoSeq)(implicit ctx: Context): Tree = {
      ref(AutoCollectionsModule)
        .select("wrappedString".toTermName)
        .appliedToArgs(seq.elems)
    }
  }

  class WrappedArray extends Rule[AutoSeq] {

    def path = "scala.collection.mutable.WrappedArray"

    def isSatisfied(seq: AutoSeq, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      (seq.semantic == Immutable || seq.semantic == Mutable) &&
      methods.containsSome(apply, isDefinedAt, update) &&
      isKnownType(seq.tpParam)
    }

    override def tree(seq: AutoSeq)(implicit ctx: Context): Tree = {
      ref(AutoCollectionsModule)
        .select("wrappedArray".toTermName)
        .appliedToType(seq.tpParam)
        .appliedToArgs(seq.elems)
        .appliedTo(classTag(seq.tpParam))
    }
  }

  class Range extends Rule[AutoSeq] {

    def path = "scala.collection.immutable.Range"

    final val MinRangeSize = 2

    def isSatisfied(seq: AutoSeq, methods: Set[Name])(implicit ctx: Context): Boolean = {
      def isNumber(tp: Type) = tp =:= ctx.definitions.IntType

      seq.semantic == Immutable && isNumber(seq.tpParam) && {
        val Typed(SeqLiteral(literals), _) = seq.elems.head

        val numbers = literals.collect {
          case x: Literal => x.const.intValue
        }

        def allNumbers = numbers.size == literals.size

        numbers.size >= MinRangeSize && allNumbers && {
          val steps = (numbers.tail, numbers).zipped.map(_ - _).distinct
          steps.size == 1
        }
      }
    }

    override def tree(seq: AutoSeq)(implicit ctx: Context): Tree = {
      def lit(n: Int) = Literal(Constant(n))

      val Typed(SeqLiteral(literals), _) = seq.elems.head

      val elems = literals.map {
        case x: Literal => x.const.intValue
      }

      val step = elems(1) - elems(0)
      val start = elems.head
      val end = elems.last

      // overload resolution
      val inclusive = module.info.member("inclusive".toTermName)
        .suchThat(_.info.firstParamTypes.size == 3).symbol

      ref(module)
        .select(inclusive)
        .appliedTo(lit(start), lit(end), lit(step))
    }
  }

  class Vector extends Rule[AutoSeq] {

    def path = "scala.collection.immutable.Vector"

    def isSatisfied(seq: AutoSeq, methods: Set[Name])(implicit ctx: Context): Boolean =
      seq.semantic == Immutable
  }

  class UnrolledBuffer extends Rule[AutoSeq] {

    def path = "scala.collection.mutable.UnrolledBuffer"

    def isSatisfied(seq: AutoSeq, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      seq.semantic == Mutable &&
      isKnownType(seq.tpParam) &&
      methods.excludes(apply, isDefinedAt, update)
    }

    override def tree(seq: AutoSeq)(implicit ctx: Context): Tree = {
      ref(module)
        .select(nme.apply)
        .appliedToTypes(seq.tpParams)
        .appliedToArgs(seq.elems)
        .appliedTo(classTag(seq.tpParam))
    }
  }

  class ListBuffer extends Rule[AutoSeq] {

    def path = "scala.collection.mutable.ListBuffer"

    def isSatisfied(seq: AutoSeq, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      seq.semantic == Mutable && methods.excludes(apply, isDefinedAt, update)
    }
  }

  // ------------ Map ------------

  class HashMap extends Rule[AutoMap] {

    def path = "scala.collection.immutable.HashMap"

    def isSatisfied(map: AutoMap, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      map.semantic == Immutable && methods.containsSome(plus, `++`)
    }
  }

  class MutableAnyRefMap extends Rule[AutoMap] {

    def path = "scala.collection.mutable.AnyRefMap"

    def isSatisfied(map: AutoMap, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      (map.semantic == Immutable || map.semantic == Mutable) &&
      map.keysTpe <:< ctx.definitions.AnyRefType &&
      methods.excludes(plus, `++`)
    }
  }

  class MutableLongMap extends Rule[AutoMap] {

    def path = "scala.collection.mutable.LongMap"

    def isSatisfied(map: AutoMap, methods: Set[Name])(implicit ctx: Context): Boolean = {
      import Methods._

      (map.semantic == Immutable || map.semantic == Mutable) &&
      map.keysTpe =:= ctx.definitions.LongType &&
      methods.excludes(plus, `++`)
    }

    override def tree(map: AutoMap)(implicit ctx: Context): Tree = {
      ref(module)
        .select(nme.apply)
        .appliedToType(map.valuesTpe)
        .appliedToArgs(map.elems)
    }
  }

  class MutableHashMap extends Rule[AutoMap] {

    def path = "scala.collection.mutable.HashMap"

    def isSatisfied(map: AutoMap, methods: Set[Name])(implicit ctx: Context): Boolean =
      map.semantic == Mutable
  }

  // ------------ Set ------------

  class HashSet extends Rule[AutoSet] {

    def path = "scala.collection.immutable.HashSet"

    def isSatisfied(set: AutoSet, methods: Set[Name])(implicit ctx: Context): Boolean =
      set.semantic == Immutable
  }

  class MutableHashSet extends Rule[AutoSet] {

    def path = "scala.collection.mutable.HashSet"

    def isSatisfied(set: AutoSet, methods: Set[Name])(implicit ctx: Context): Boolean =
      set.semantic == Mutable
  }

  /** A class tag can be generated */
  private def isKnownType(tpe: Type)(implicit ctx: Context) =
    !TypeErasure.isUnboundedGeneric(tpe)

  private def classTag(tpe: Type)(implicit ctx: Context) = {
    val classTagModule = ctx.requiredModule("scala.reflect.ClassTag")
    ref(classTagModule)
      .select(nme.apply)
      .appliedToType(tpe)
      .appliedTo(clsOf(tpe))
  }

  private def AutoCollectionsModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.AutoCollections")

  private object Methods {
    val apply       = "apply".toTermName
    val isDefinedAt = "isDefinedAt".toTermName
    val head        = "head".toTermName
    val tail        = "tail".toTermName
    val prepend     = "+:".toTermName.encode
    val append      = ":+".toTermName.encode
    val plus        = "+".toTermName.encode
    val `++`        = "++".toTermName.encode
    val update      = "update".toTermName
    val isEmpty     = "isEmpty".toTermName
    val nonEmpty    = "nonEmpty".toTermName
  }

  private implicit class SetOps[T](val set: Set[T]) extends AnyVal {
    def containsAll(args: T*):  Boolean = args.forall(set.contains)
    def containsSome(args: T*): Boolean = args.exists(set.contains)
    def containsOnly(args: T*): Boolean = set.subsetOf(args.toSet)
    def excludes(args: T*):     Boolean = args.forall(!set.contains(_))
  }
}
