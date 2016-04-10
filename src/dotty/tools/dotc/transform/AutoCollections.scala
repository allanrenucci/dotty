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
class AutoCollections extends MiniPhaseTransform {

  import AutoCollections._
  import tpd._

  override def phaseName: String = "autocollections"

  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val autoCollections = ctx.preAutoCollectionPhase.asInstanceOf[PreAutoCollections].anonClasses

    autoCollections.get(tree).fold(tree: Tree) {
      case (args, tpArgs) =>
        val impl = pickImplementation(tree.tpe)
        ref(impl)
          .select(nme.apply)
          .appliedToTypes(tpArgs)
          .appliedToArgs(args)
    }
  }

  private def methodCalls(sym: Symbol)(implicit ctx: Context): Set[Symbol] = {
    val methods = ctx.buildCallGraphPhase.asInstanceOf[BuildCallGraph].methods

    methods.filter(_.call.asInstanceOf[TermRef].prefix.classSymbol == sym)
      .map(_.call.termSymbol)
  }

  private def defaultStrategy(implicit ctx: Context) = Map(
    AutoSeqType.classSymbol -> VectorModule,
    AutoMapType.classSymbol -> HashMapModule,
    AutoSetType.classSymbol -> HashSetModule
  )

  private def pickImplementation(tpe: Type)(implicit ctx: Context): Symbol = {
    val meths = methodCalls(tpe.classSymbol).filterNot(_.isConstructor)
    val collectionTpe = tpe.parents.head

    //    if (collectionTpe =:= AutoSeqType) {
    //      def method(name: String) = SeqClass.requiredMethod(name.toTermName.encode)
    //
    //      println(tpe.classSymbol)
    //      println(meths)
    //
    //      if (meths == Set(method("head"), method("tail")))
    //        ListBufferModule
    //      else if ((meths contains method("+:")) || (meths contains method(":+")))
    //        QueueModule
    //      else if ((meths contains method("apply")) || (meths contains method("isDefinedAt")))
    //        ArrayModule
    //      else VectorModule
    //
    //    } else if (collectionTpe =:= AutoMapType) {
    //      def method(name: String) = MapClass.requiredMethod(name.toTermName.encode)
    //
    //      if (meths contains method("+"))
    //        HashMapModule
    //      else
    //        HashMapModule
    //
    //    } else if (collectionTpe =:= AutoSetType) {
    //      def method(name: String) = SetClass.requiredMethod(name.toTermName.encode)
    //
    //      HashSetModule
    //    }
    //
    //    else throw new IllegalStateException(s"Unsupported collection $collectionTpe")

    defaultStrategy.apply(collectionTpe.classSymbol)
  }
}

object AutoCollections {

  // Seq
  def SeqClass(implicit ctx: Context) =
    ctx.requiredClass("scala.collection.Seq")

  def AutoSeqType(implicit ctx: Context) =
    ctx.requiredClassRef("scala.collection.AutoCollections.AutoSeq")
  def AutoSeqApply(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.AutoCollections.AutoSeq").requiredMethod(nme.apply)

  def ListModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.immutable.List")
  def ListBufferModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.mutable.ListBuffer")
  def QueueModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.immutable.Queue")
  def ArrayModule(implicit ctx: Context) =
    ctx.requiredModule("Array")
  def VectorModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.immutable.Vector")

  // Map
  def MapClass(implicit ctx: Context) =
    ctx.requiredClass("scala.collection.Map")

  def AutoMapType(implicit ctx: Context) =
    ctx.requiredClassRef("scala.collection.AutoCollections.AutoMap")
  def AutoMapApply(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.AutoCollections.AutoMap").requiredMethod(nme.apply)

  def HashMapModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.immutable.HashMap")

  // Set
  def SetClass(implicit ctx: Context) =
    ctx.requiredClass("scala.collection.Set")

  def AutoSetApply(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.AutoCollections.AutoSet").requiredMethod(nme.apply)
  def AutoSetType(implicit ctx: Context) =
    ctx.requiredClassRef("scala.collection.AutoCollections.AutoSet")

  def HashSetModule(implicit ctx: Context) =
    ctx.requiredModule("scala.collection.immutable.HashSet")
}
