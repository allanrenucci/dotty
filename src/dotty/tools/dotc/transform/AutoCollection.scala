package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.transform.Summaries.CallWithContext
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** A transformation which substitutes an auto collection by an implementation
  * following the following rules:
  *
  * Immutable
  *
  * - AutoSeq
  *   - [[scala.collection.mutable.ListBuffer]] if `head` and `tail` are the only operations
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
  */
class AutoCollection extends MiniPhaseTransform {

  import AutoCollection._
  import tpd._

  override def phaseName: String = "autocollection"

  private def methods(implicit ctx: Context): Set[CallWithContext] =
    ctx.buildCallGraphPhase.asInstanceOf[BuildCallGraph].methods

  private def methodCalls(sym: Symbol)(implicit context: Context): Set[Symbol] =
    methods.filter(_.call.asInstanceOf[TermRef].prefix.classSymbol == sym)
      .map(_.call.termSymbol)

  private def autoCollections(implicit ctx: Context) =
    ctx.preAutoCollectionPhase.asInstanceOf[PreAutoCollection].anonClasses

  private def pickImplementation(tpe: Type)(implicit ctx: Context): Symbol = {
    val strategy = Map(
      AutoSeqType.classSymbol -> ListModule,
      AutoMapType.classSymbol -> HashMapModule,
      AutoSetType.classSymbol -> HashSetModule
    )

    val meths = methodCalls(tpe.classSymbol)
    val superTpe = tpe.parents.head

    strategy(superTpe.classSymbol)
  }

  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = {
    autoCollections.get(tree).fold(tree: Tree) {
      case (args, tpArgs) =>
        val impl = pickImplementation(tree.tpe)
        ref(impl)
          .select(nme.apply)
          .appliedToTypes(tpArgs)
          .appliedToArgs(args)
    }
  }
}

object AutoCollection {

  // Seq
  def AutoSeqType(implicit ctx: Context) =
    ctx.requiredClassRef("scala.collection.AutoCollections.AutoSeq")
  def AutoSeqApply(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.AutoCollections.AutoSeq").requiredMethod(nme.apply)
  def ListModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.immutable.List")

  // Map
  def AutoMapType(implicit ctx: Context) =
    ctx.requiredClassRef("scala.collection.AutoCollections.AutoMap")
  def AutoMapApply(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.AutoCollections.AutoMap").requiredMethod(nme.apply)
  def HashMapModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.immutable.HashMap")

  // Set
  def AutoSetApply(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.AutoCollections.AutoSet").requiredMethod(nme.apply)
  def AutoSetType(implicit ctx: Context) =
    ctx.requiredClassRef("scala.collection.AutoCollections.AutoSet")
  def HashSetModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.immutable.HashSet")
}
