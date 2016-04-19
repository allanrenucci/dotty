package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.{immutable, mutable}

/** A transformation which substitutes an auto collection by an implementation
  * following the following rules:
  *
  * Immutable
  *
  * - AutoSeq
  *   - [[mutable.ListBuffer]] if `head` and `tail` are the only operations
  *   - [[immutable.Queue]] if `:+` and `+:` are used
  *   - [[Array]] if operations are by index accesses (e.g. `apply`), unless elements are [[Char]] then use [[String]]
  *   - [[immutable.Range]] if all elements are numbers with a constant delta
  *   - [[immutable.Vector]] otherwise
  *
  * - AutoMap
  *   - [[immutable.HashMap]] if elements are added
  *   - If no element are added
  *     - [[mutable.AnyRefMap]] if the type parameters does not include primitives
  *     - [[immutable.LongMap]] if the keys' type is [[Long]]
  *     - [[mutable.HashMap]] otherwise
  *   // We can consider adding an option of creating a sortedMap
  *   // in this case, if someone actually uses operations that reveal sorting
  *   // we need to create a [[immutable.TreeMap]], otherwise follow the algorithm above
  *
  * - AutoSet
  *   - [[immutable.BitSet]] if elements' type is [[Int]]
  *   - [[immutable.HashSet]] otherwise
  *
  * Mutable
  *
  * - AutoSeq
  *   - [[Array]] if operations are by index accesses (e.g. `apply`)
  *   - otherwise:
  *     - [[mutable.UnrolledBuffer]] if type is known
  *     - [[mutable.ListBuffer]] otherwise
  *
  * - AutoMap
  *   - [[mutable.HashMap]]
  *
  * - AutoSet
  *   - [[mutable.BitSet]] if elements' type is [[Int]]
  *   - [[mutable.HashSet]] otherwise
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

    autoCollections.get(tree).fold(tree: Tree)(pickImplementation)
  }

  private def pickImplementation(collection: AutoCollection)(implicit ctx: Context): Tree = {

    import Methods._

    def appliedTo(impl: Symbol, tpParams: List[Type] = collection.tpParams, args: List[Tree] = collection.elems) = {
      ref(impl)
        .select(nme.apply)
        .appliedToTypes(tpParams)
        .appliedToArgs(args)
    }

    val methods: Set[Name] = methodCalls(collection.symbol)
      .filterNot(_.isConstructor)
      .map(_.name)

    collection match {
      // ------------ Immutable ------------

      // if `head` and `tail` are the only operations
      case AutoSeq(Immutable) if methods.containsOnly(head, tail) =>
        appliedTo(ListBuffer)

      // if `:+` and `+:` are used
      case AutoSeq(Immutable) if methods.containsSome(`+:`, `:+`) =>
        appliedTo(ImmutableQueue)

      // if operations are by index accesses
      case AutoSeq(Immutable) if methods.containsSome(apply, isDefinedAt) =>
        appliedTo(Array)
        ??? // FIXME: Array(xs: T*)(implicit arg0: classTag[T])

      // TODO: Range if all elements are numbers with a constant delta

      // default
      case AutoSeq(Immutable) =>
        appliedTo(Vector)

      // if elements are added
      case AutoMap(Immutable) if methods.containsSome(plus, `++`) =>
        appliedTo(ImmutableHashMap)

      // TODO: AnyRefMap if the type parameters does not include primitives

      //  if the keys' type is Long
      case map @ AutoMap(Immutable) if map.keysTpe =:= ctx.definitions.LongType =>
        appliedTo(LongMap, map.valuesTpe :: Nil)

      // default
      case map @ AutoMap(Immutable) =>
        appliedTo(MutableHashMap)

      // if elements' type is Int
      case set @ AutoSet(Immutable) if set.tpParam =:= ctx.definitions.IntType =>
        appliedTo(ImmutableBitSet, Nil)

      // default
      case AutoSet(Immutable) =>
        appliedTo(ImmutableHashSet)


      // ------------ Mutable ------------

      // if operations are by index accesses
      case AutoSeq(Mutable) if methods.containsSome(apply, isDefinedAt, update) =>
        appliedTo(Array)
        ??? // FIXME: Array(xs: T*)(implicit arg0: classTag[T])

      // TODO: UnrolledBuffer(xs: T*)(implicit arg0: classTag[T]) if type is known

      // default
      case AutoSeq(Mutable) =>
        appliedTo(ListBuffer)

      case AutoMap(Mutable) =>
        appliedTo(MutableHashMap)

      // if elements' type is Int
      case set @ AutoSet(Mutable) if set.tpParam =:= ctx.definitions.IntType =>
        appliedTo(MutableBitSet, Nil)

      // default
      case AutoSet(Mutable) =>
        appliedTo(MutableHashSet)


      // ------------ Lazy ------------
      case AutoSeq(Lazy) => ???
      case AutoMap(Lazy) => ???
      case AutoSet(Lazy) => ???
    }
  }

  private def methodCalls(sym: Symbol)(implicit ctx: Context): Set[Symbol] = {
    val methods = ctx.buildCallGraphPhase.asInstanceOf[BuildCallGraph].methods

    methods
      .filter(_.call.asInstanceOf[TermRef].prefix.classSymbol == sym)
      .map(_.call.termSymbol)
  }

}

object AutoCollections {

  import tpd._

  sealed trait Semantic
  case object Immutable extends Semantic
  case object Mutable   extends Semantic
  case object Lazy      extends Semantic

  abstract class AutoCollection(val symbol: Symbol,
                                val semantic: Semantic,
                                val tpParams: List[Type],
                                val elems: List[Tree])

  class AutoSeq(symbol: Symbol,
                semantic: Semantic,
                tpParams: List[Type],
                elems: List[Tree]) extends AutoCollection(symbol, semantic, tpParams, elems) {

    require(tpParams.size == 1)

    def tpParam = tpParams.head
  }

  object AutoSeq { def unapply(seq: AutoSeq) = Some(seq.semantic) }

  class AutoMap(symbol: Symbol,
                semantic: Semantic,
                tpParams: List[Type],
                elems: List[Tree]) extends AutoCollection(symbol, semantic, tpParams, elems) {

    require(tpParams.size == 2)

    def keysTpe = tpParams.head
    def valuesTpe = tpParams(1)
  }

  object AutoMap { def unapply(map: AutoMap) = Some(map.semantic) }

  class AutoSet(symbol: Symbol,
                semantic: Semantic,
                tpParams: List[Type],
                elems: List[Tree]) extends AutoCollection(symbol, semantic, tpParams, elems) {

    require(tpParams.size == 1)

    def tpParam = tpParams.head
  }

  object AutoSet { def unapply(set: AutoSet) = Some(set.semantic) }

  def IterableClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Iterable")

  // ------------ Seq ------------
  def SeqClass(implicit ctx: Context)        = ctx.requiredClass("scala.collection.Seq")
  def MutableSeqClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.mutable.Seq")


  def ListBuffer(implicit ctx: Context)     = ctx.requiredModule("scala.collection.mutable.ListBuffer")
  def ImmutableQueue(implicit ctx: Context) = ctx.requiredModule("scala.collection.immutable.Queue")
  def Array(implicit ctx: Context)          = ctx.requiredModule("scala.Array")
  def Vector(implicit ctx: Context)         = ctx.requiredModule("scala.collection.immutable.Vector")
  def MutableQueue(implicit ctx: Context)   = ctx.requiredModule("scala.collection.mutable.Queue")

  // ------------ Map ------------
  def MapClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Map")

  def ImmutableHashMap(implicit ctx: Context) = ctx.requiredModule("scala.collection.immutable.HashMap")
  def MutableHashMap(implicit ctx: Context)   = ctx.requiredModule("scala.collection.mutable.HashMap")
  def AnyRefMap(implicit ctx: Context)        = ctx.requiredModule("scala.collection.mutable.AnyRefMap")
  def LongMap(implicit ctx: Context)          = ctx.requiredModule("scala.collection.immutable.LongMap")

  // ------------ Set ------------
  def SetClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Set")

  def ImmutableHashSet(implicit ctx: Context) = ctx.requiredModule("scala.collection.immutable.HashSet")
  def MutableHashSet(implicit ctx: Context)   = ctx.requiredModule("scala.collection.mutable.HashSet")
  def ImmutableBitSet(implicit ctx: Context)  = ctx.requiredModule("scala.collection.immutable.BitSet")
  def MutableBitSet(implicit ctx: Context)    = ctx.requiredModule("scala.collection.mutable.BitSet")


  object Methods {
    val apply       = "apply".toTermName
    val isDefinedAt = "isDefinedAt".toTermName
    val head        = "head".toTermName
    val tail        = "tail".toTermName
    val `+:`        = "+:".toTermName.encode
    val `:+`        = "+:".toTermName.encode
    val plus        = "+".toTermName.encode
    val `++`        = "++".toTermName.encode
    val update      = "update".toTermName
  }

  private implicit class SetOps[T](val set: Set[T]) extends AnyVal {
    def containsSome(args: T*): Boolean = args.exists(set.contains)
    def containsOnly(args: T*): Boolean = set == args.toSet
  }
}
