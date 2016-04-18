package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** A transformation which substitutes an auto collection by an implementation
  * following the following rules:
  *
  * Immutable
  *
  * - AutoSeq
  *   - [[scala.collection.mutable.UnrolledBuffer]] if `head` and `tail` are the only operations
  *   - [[scala.collection.immutable.Queue]] if `:+` and `+:` are used
  *   - [[Array]] if operations are by index accesses (e.g. `apply`), unless elements are [[Char]] then use [[String]]
  *   - [[scala.collection.immutable.Range]] if all elements are numbers with a constant delta
  *   - [[scala.collection.immutable.Vector]] otherwise
  *
  * - AutoMap
  *   - [[scala.collection.immutable.HashMap]] if elements are added
  *   - If no element are added
  *     - [[scala.collection.mutable.AnyRefMap]] if the types parameters does not include primitives
  *     - [[scala.collection.immutable.LongMap]] if the keys' type is [[Long]]
  *     - [[scala.collection.mutable.HashMap]] otherwise
  *   // We can consider adding an option of creating a sortedMap
  *   // in this case, if someone actually uses operations that reveal sorting
  *   // we need to create a [[scala.collection.immutable.TreeMap]], otherwise follow the algorithm above
  *
  * - AutoSeq
  *
  * Mutable
  *
  * - AutoSeq
  *   - Array for methods using indexes
  *   - push and pop Queue
  *   - character StringBuilder
  *
  * - AutoMap / AutoSet
  *   - HashMap / HashSet
  *   - BitMap for Boolean / BitSet
  *
  * Lazy
  *   ???
  */
class AutoCollections extends MiniPhaseTransform {

  import AutoCollections._
  import tpd._

  override def phaseName: String = "autocollections"

  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val autoCollections = ctx.preAutoCollectionPhase.asInstanceOf[PreAutoCollections].instances

    autoCollections.get(tree).fold(tree: Tree) { coll =>
      val (impl, tpParams, args) = pickImplementation(coll)
      ref(impl)
        .select(nme.apply)
        .appliedToTypes(tpParams)
        .appliedToArgs(args)
    }
  }

  private def pickImplementation(collection: AutoCollection)(implicit ctx: Context): (Symbol, List[Type], List[Tree]) = {
    val methods = methodCalls(collection.symbol).filterNot(_.isConstructor)

    collection match {
      case seq @ AutoSeq(Immutable) =>
        def method(name: String) = SeqClass.requiredMethod(name.toTermName.encode)

        println(methods)
        println(methods contains method("apply"))

        val impl =
          if (methods == Set(method("head"), method("tail")))
            ListBuffer
          else if ((methods contains method("+:")) || (methods contains method(":+")))
            ImmutableQueue
          else if ((methods contains method("apply")) || (methods contains method("isDefinedAt")))
            Array
          else Vector

        (impl, seq.elemsTpe :: Nil, seq.elems)

      case seq @ AutoSeq(Mutable) =>
        def method(name: String) = MutableSeqClass.requiredMethod(name.toTermName.encode)
        (Array, seq.elemsTpe :: Nil, seq.elems)

      case AutoSeq(Lazy) =>
        ???

      case map @ AutoMap(Immutable) =>
        def method(name: String) = MapClass.requiredMethod(name.toTermName.encode)

        val impl =
          if (methods contains method("+"))
            ImmutableHashMap
          else
            ImmutableHashMap

        (impl, map.keysTpe :: map.valuesTpe :: Nil, map.elems)

      case map @ AutoMap(Mutable) =>
        (MutableHashMap, map.keysTpe :: map.valuesTpe :: Nil, map.elems)

      case AutoMap(Lazy) =>
        ???

      case set @ AutoSet(Immutable) =>
        def method(name: String) = SetClass.requiredMethod(name.toTermName.encode)
        (ImmutableHashSet, set.elemsTpe :: Nil, set.elems)

      case set @ AutoSet(Mutable) =>
        (MutableHashSet, set.elemsTpe :: Nil, set.elems)

      case AutoSet(Lazy) =>
        ???
    }
  }

  private def methodCalls(sym: Symbol)(implicit ctx: Context): Set[Symbol] = {
    val methods = ctx.buildCallGraphPhase.asInstanceOf[BuildCallGraph].methods

    methods.filter(_.call.asInstanceOf[TermRef].prefix.classSymbol == sym)
      .map(_.call.termSymbol)
  }

}

object AutoCollections {

  import tpd._

  sealed trait Semantic
  case object Immutable extends Semantic
  case object Mutable   extends Semantic
  case object Lazy      extends Semantic

  abstract class AutoCollection(val symbol: Symbol)

  class AutoSeq(val semantic: Semantic,
                val elemsTpe: Type,
                val elems: List[Tree],
                symbol: Symbol) extends AutoCollection(symbol)

  object AutoSeq { def unapply(seq: AutoSeq) = Some(seq.semantic) }

  class AutoMap(val semantic: Semantic,
                val keysTpe: Type,
                val valuesTpe: Type,
                val elems: List[Tree],
                symbol: Symbol)  extends AutoCollection(symbol)

  object AutoMap { def unapply(map: AutoMap) = Some(map.semantic) }

  class AutoSet(val semantic: Semantic,
                val elemsTpe: Type,
                val elems: List[Tree],
                symbol: Symbol) extends AutoCollection(symbol)

  object AutoSet { def unapply(set: AutoSet) = Some(set.semantic) }

  def IterableClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Iterable")

  // ------------ Seq ------------
  def SeqClass(implicit ctx: Context)        = ctx.requiredClass("scala.collection.Seq")
  def MutableSeqClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.mutable.Seq")


  def ListBuffer(implicit ctx: Context)     = ctx.requiredModule("scala.collection.mutable.ListBuffer")
  def ImmutableQueue(implicit ctx: Context) = ctx.requiredModule("scala.collection.immutable.Queue")
  def Array(implicit ctx: Context)          = ctx.requiredModule("Array")
  def Vector(implicit ctx: Context)         = ctx.requiredModule("scala.collection.immutable.Vector")
  def MutableQueue(implicit ctx: Context)   = ctx.requiredModule("scala.collection.mutable.Queue")

  // ------------ Map ------------
  def MapClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Map")

  def ImmutableHashMap(implicit ctx: Context) = ctx.requiredModule("scala.collection.immutable.HashMap")
  def MutableHashMap(implicit ctx: Context)   = ctx.requiredModule("scala.collection.mutable.HashMap")

  // ------------ Set ------------
  def SetClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Set")

  def ImmutableHashSet(implicit ctx: Context) = ctx.requiredModule("scala.collection.immutable.HashSet")
  def MutableHashSet(implicit ctx: Context)   = ctx.requiredModule("scala.collection.mutable.HashSet")
}
