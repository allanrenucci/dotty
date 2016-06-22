package dotty.tools.dotc.transform.autocollections

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.sourcePos
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.BuildCallGraph
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import dotty.tools.dotc.transform.autocollections.Rules._

import scala.collection.{immutable, mutable}

/** A transformation which substitutes an auto collection by an implementation
  * following a set of rules
  */
class AutoCollections extends MiniPhaseTransform {

  import AutoCollections._
  import tpd._

  override def phaseName: String = "autocollections"

  /** Rules for `Seq`
    *
    * Immutable:
    *   - [[immutable.Range]] if all elements are numbers with a constant delta
    *   - [[immutable.List]] if `head`, `tail`, `prepend`, `nonEmpty`, `isEmpty`
    *      are the only operations
    *   - [[immutable.Queue]] if operations contains `append`
    *   - [[mutable.WrappedArray]] if operations are by index accesses (e.g. `apply`),
    *      unless elements are [[Char]] then use [[immutable.WrappedString]]
    *   - [[immutable.Vector]] otherwise
    *
    * Mutable:
    *   - [[mutable.WrappedArray]] if operations are by index accesses (e.g. `apply`)
    *   - otherwise:
    *     - [[mutable.UnrolledBuffer]] if type is known
    *     - [[mutable.ListBuffer]] otherwise
    */
  val seqRules = List(
    new Range,
    new ListR,
    new Queue,
    new WrappedString,
    new WrappedArray,
    new Vector,
    new UnrolledBuffer,
    new ListBuffer
  )

  /** Rules for `Map`
    *
    * Immutable:
    *   - [[immutable.HashMap]] if elements are added
    *   - If no element are added
    *     - [[mutable.AnyRefMap]] if keys' type is subtype of [[AnyRef]]
    *     - [[mutable.LongMap]] if keys' type is [[Long]]
    *     - [[mutable.HashMap]] otherwise
    *   // We can consider adding an option of creating a sortedMap
    *   // in this case, if someone actually uses operations that reveal sorting
    *   // we need to create a [[immutable.TreeMap]], otherwise follow the algorithm above
    *
    *   Mutable:
    *     - [[mutable.AnyRefMap]] if keys' type is subtype of [[AnyRef]]
    *     - [[mutable.LongMap]] if keys' type is [[Long]]
    */
  val mapRules = List(
    new HashMap,
    new MutableAnyRefMap,
    new MutableLongMap,
    new MutableHashMap
  )

  /** Rules for `Set`
    *
    * Immutable:
    *   - [[immutable.HashSet]]
    *
    * Mutable:
    *   - [[mutable.HashSet]]
    */
  val setRules = List(
    new HashSet,
    new MutableHashSet
  )


  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val autoCollections = ctx.preAutoCollectionPhase.asInstanceOf[PreAutoCollections].instances

    autoCollections.get(tree).fold(tree: Tree) { collection =>

      val methods: Set[Name] = methodCalls(collection.symbol)
        .filterNot(_.isConstructor)
        .map(_.name)


      val impl = collection match {
        case seq: AutoSeq => evaluate(seqRules, seq, methods)
        case map: AutoMap => evaluate(mapRules, map, methods)
        case set: AutoSet => evaluate(setRules, set, methods)
      }

      println("-" * 60)
      println(s"Methods called: ${methods.mkString(", ")}")
      println {
        val sym = impl.fold("Not found")(_.tpe.classSymbol.toString)
        s"Implementation: $sym"
      }
      println("-" * 60)

      impl.getOrElse {
        ctx.error("No rule satisfied for this use case", tree.pos)
        tree
      }
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
                elems: List[Tree])
    extends AutoCollection(symbol, semantic, tpParams, elems) {

    require(tpParams.size == 1)

    def tpParam = tpParams.head
  }

  class AutoMap(symbol: Symbol,
                semantic: Semantic,
                tpParams: List[Type],
                elems: List[Tree])
    extends AutoCollection(symbol, semantic, tpParams, elems) {

    require(tpParams.size == 2)

    def keysTpe = tpParams.head
    def valuesTpe = tpParams(1)
  }

  class AutoSet(symbol: Symbol,
                semantic: Semantic,
                tpParams: List[Type],
                elems: List[Tree])
    extends AutoCollection(symbol, semantic, tpParams, elems) {

    require(tpParams.size == 1)

    def tpParam = tpParams.head
  }
}
