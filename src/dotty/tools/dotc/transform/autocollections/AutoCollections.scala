package dotty.tools.dotc.transform.autocollections

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.{StringDecorator, sourcePos}
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import dotty.tools.dotc.transform.BuildCallGraph

import scala.collection.{immutable, mutable}

/** A transformation which substitutes an auto collection by an implementation
  * following the following rules:
  *
  * Immutable
  *
  * - AutoSeq
  *   - [[mutable.ListBuffer]] if `head` and `tail` are the only operations
  *   - [[immutable.Queue]] if `:+` or `+:` are used
  *   - [[mutable.WrappedArray]] if operations are by index accesses (e.g. `apply`),
  *      unless elements are [[Char]] then use [[immutable.WrappedString]]
  *   - [[immutable.Range]] if all elements are numbers with a constant delta
  *   - [[immutable.Vector]] otherwise
  *
  * - AutoMap
  *   - [[immutable.HashMap]] if elements are added
  *   - If no element are added
  *     - [[mutable.AnyRefMap]] if keys' type is subtype of [[AnyRef]]
  *     - [[immutable.LongMap]] if keys' type is [[Long]]
  *     - [[mutable.HashMap]] otherwise
  *   // We can consider adding an option of creating a sortedMap
  *   // in this case, if someone actually uses operations that reveal sorting
  *   // we need to create a [[immutable.TreeMap]], otherwise follow the algorithm above
  *
  * - AutoSet
  *   - [[immutable.HashSet]]
  *
  * Mutable
  *
  * - AutoSeq
  *   - [[mutable.WrappedArray]] if operations are by index accesses (e.g. `apply`)
  *   - otherwise:
  *     - [[mutable.UnrolledBuffer]] if type is known
  *     - [[mutable.ListBuffer]] otherwise
  *
  * - AutoMap
  *   - [[mutable.HashMap]]
  *
  * - AutoSet
  *   - [[mutable.HashSet]]
  *
  * Lazy
  *   - Not yet supported
  */
class AutoCollections extends MiniPhaseTransform {

  import AutoCollections._
  import tpd._

  override def phaseName: String = "autocollections"


  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val autoCollections = ctx.preAutoCollectionPhase.asInstanceOf[PreAutoCollections].instances

    autoCollections.get(tree).fold(tree: Tree) { collection =>
      val impl = pickImplementation(collection)

      val methods: Set[Name] = methodCalls(collection.symbol)
        .filterNot(_.isConstructor)
        .map(_.name)

      println("-" * 60)
      println(s"Methods called: ${methods.mkString(", ")}")
      println(impl.tpe.classSymbol)
      println("-" * 60)

      if (impl.isEmpty) {
        ctx.error("Lazy collections are not supported", tree.pos)
        tree
      }
      else impl
    }
  }

  private def pickImplementation(collection: AutoCollection)(implicit ctx: Context): Tree = {

    import Methods._

    /** Default collection builder
      *
      * @return a tree of the form `module.apply[T](elems)`
      */
    def buildCollection(collection: AutoCollection)
                       (module: Symbol,
                        tpParams: List[Type] = collection.tpParams,
                        args: List[Tree] = collection.elems) = {
      ref(module)
        .select(nme.apply)
        .appliedToTypes(tpParams)
        .appliedToArgs(args)
    }

    def classTag(tpe: Type) = {
      val classTagModule = ctx.requiredModule("scala.reflect.ClassTag")
      ref(classTagModule)
        .select(nme.apply)
        .appliedToType(tpe)
        .appliedTo(clsOf(tpe))
    }

    def wrappedArray(seq: AutoSeq) = {
      val tpe = seq.tpParam

      ref(AutoCollectionModule)
        .select(Methods.wrappedArray)
        .appliedToType(tpe)
        .appliedToArgs(seq.elems)
        .appliedTo(classTag(tpe))
    }

    val methods: Set[Name] = methodCalls(collection.symbol)
      .filterNot(_.isConstructor)
      .map(_.name)

    collection match {
      // ------------ Immutable ------------

      // if `head` and `tail` are the only operations
      case AutoSeq(Immutable) if methods.containsOnly(head, tail) =>
        buildCollection(collection)(ListBufferModule)

      // if `:+` or `+:` are used
      case AutoSeq(Immutable) if methods.containsSome(`+:`, `:+`) =>
        buildCollection(collection)(ImmutableQueueModule)

      // if operations are by index accesses
      case seq @ AutoSeq(Immutable) if methods.containsSome(apply, isDefinedAt) =>
        if (seq.tpParam =:= ctx.definitions.CharType) {
          ref(AutoCollectionModule)
            .select(wrappedString)
            .appliedToArgs(seq.elems)
        }
        else if (!TypeErasure.isUnboundedGeneric(seq.tpParam)) wrappedArray(seq)
        else buildCollection(seq)(VectorModule)


      case seq @ AutoSeq(Immutable) if seq.tpParam =:= ctx.definitions.IntType =>
        // Range if all elements are numbers with a constant delta
        val rangeOption = {
          val Typed(SeqLiteral(literals), _) = collection.elems.head

          val MinRangeSize = 2

          if (literals.size >= MinRangeSize && literals.forall(_.isInstanceOf[Literal])) {
            val elems = literals.map {
              case lit: Literal => lit.const.intValue
            }

            val steps = (elems.tail, elems).zipped.map(_ - _).distinct

            steps match {
              case List(step) =>
                def lit(n: Int) = Literal(Constant(n))
                val start = elems.head
                val end = elems.last
                // overload resolution
                val inclusive = RangeModule.info.member(Methods.inclusive)
                  .suchThat(_.info.firstParamTypes.size == 3).symbol
                val range = ref(RangeModule)
                  .select(inclusive)
                  .appliedTo(lit(start), lit(end), lit(step))
                Some(range)

              case _ => None
            }
          } else None
        }

        rangeOption.getOrElse {
          buildCollection(collection)(VectorModule)
        }

      // default
      case AutoSeq(Immutable) =>
        buildCollection(collection)(VectorModule)

      // if elements are added
      case AutoMap(Immutable) if methods.containsSome(plus, `++`) =>
        buildCollection(collection)(ImmutableHashMapModule)

      // if keys' type is subtype of `AnyReF`
      case map @ AutoMap(Immutable) if map.keysTpe <:< ctx.definitions.AnyRefType =>
        buildCollection(map)(AnyRefMapModule)

      //  if keys' type is Long
      case map @ AutoMap(Immutable) if map.keysTpe =:= ctx.definitions.LongType =>
        buildCollection(map)(LongMapModule, map.valuesTpe :: Nil)

      // default
      case AutoMap(Immutable) =>
        buildCollection(collection)(MutableHashMapModule)

      // default
      case AutoSet(Immutable) =>
        buildCollection(collection)(ImmutableHashSetModule)


      // ------------ Mutable ------------

      // if operations are by index accesses
      case seq @ AutoSeq(Mutable) if methods.containsSome(apply, isDefinedAt, update) =>
        wrappedArray(seq)

      // if type is known
      case seq @ AutoSeq(Mutable) if !TypeErasure.isUnboundedGeneric(seq.tpParam) =>
        buildCollection(seq)(UnrolledBufferModule).appliedTo(classTag(seq.tpParam))

      // default
      case AutoSeq(Mutable) =>
        buildCollection(collection)(ListBufferModule)

      case AutoMap(Mutable) =>
        buildCollection(collection)(MutableHashMapModule)

      // default
      case AutoSet(Mutable) =>
        buildCollection(collection)(MutableHashSetModule)


      // ------------ Lazy ------------
      case AutoSeq(Lazy) |  AutoMap(Lazy) | AutoSet(Lazy) =>
        genericEmptyTree

    }
  }

  private def methodCalls(sym: Symbol)(implicit ctx: Context): Set[Symbol] = {
    val methods = ctx.buildCallGraphPhase.asInstanceOf[BuildCallGraph].getReachableMethods

    methods
      .filter(_.call.normalizedPrefix.classSymbol == sym)
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

  private def immutableCollectionModule(collection: String)(implicit ctx: Context) =
    ctx.requiredModule(s"scala.collection.immutable.$collection")

  private def mutableCollectionModule(collection: String)(implicit ctx: Context) =
    ctx.requiredModule(s"scala.collection.mutable.$collection")

  private def AutoCollectionModule(implicit ctx: Context) = ctx.requiredModule("scala.collection.AutoCollections")

  // ------------ Seq ------------
  private def ListBufferModule(implicit ctx: Context)     = mutableCollectionModule("ListBuffer")
  private def UnrolledBufferModule(implicit ctx: Context) = mutableCollectionModule("UnrolledBuffer")
  private def ImmutableQueueModule(implicit ctx: Context) = immutableCollectionModule("Queue")
  private def VectorModule(implicit ctx: Context)         = immutableCollectionModule("Vector")
  private def RangeModule(implicit ctx: Context)          = immutableCollectionModule("Range")

  // ------------ Map ------------
  private def MutableHashMapModule(implicit ctx: Context)   = mutableCollectionModule("HashMap")
  private def AnyRefMapModule(implicit ctx: Context)        = mutableCollectionModule("AnyRefMap")
  private def ImmutableHashMapModule(implicit ctx: Context) = immutableCollectionModule("HashMap")
  private def LongMapModule(implicit ctx: Context)          = immutableCollectionModule("LongMap")

  // ------------ Set ------------
  private def MutableHashSetModule(implicit ctx: Context)   = mutableCollectionModule("HashSet")
  private def ImmutableHashSetModule(implicit ctx: Context) = immutableCollectionModule("HashSet")


  private object Methods {
    val apply       = "apply".toTermName
    val isDefinedAt = "isDefinedAt".toTermName
    val head        = "head".toTermName
    val tail        = "tail".toTermName
    val `+:`        = "+:".toTermName.encode
    val `:+`        = ":+".toTermName.encode
    val plus        = "+".toTermName.encode
    val `++`        = "++".toTermName.encode
    val update      = "update".toTermName

    val wrappedArray  = "wrappedArray".toTermName
    val wrappedString = "wrappedString".toTermName
    val inclusive     = "inclusive".toTermName
  }

  private implicit class SetOps[T](val set: Set[T]) extends AnyVal {
    def containsSome(args: T*): Boolean = args.exists(set.contains)
    def containsOnly(args: T*): Boolean = set == args.toSet
  }
}
