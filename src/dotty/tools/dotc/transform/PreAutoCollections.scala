package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.MethodicType
import dotty.tools.dotc.transform.AutoCollections._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** A transformation which substitutes an auto collection constructor by a
  * dummy implementation in order to track the methods called on the collection
  * via call graph analysis.
  *
  * {{{
  *   AutoSeq(1, 2)
  *   AutoSeq(1, 2)(Mutable)
  *
  *   // Possible alternative syntax (not supported)
  *   new AutoSeq(1, 2) with Mutable
  * }}}
  *
  */
class PreAutoCollections extends MiniPhaseTransform {

  import PreAutoCollections._
  import tpd._

  override def phaseName: String = "preautocollections"

  var instances = Map.empty[Block, AutoCollection]

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    def isAutoCollectionConstructor(tree: Apply) = {
      val constructors = Set[Symbol](AutoSeqApply, AutoMapApply, AutoSetApply)
      (constructors contains tree.symbol) && !tree.tpe.isInstanceOf[MethodicType]
    }

    if (isAutoCollectionConstructor(tree)) transform(tree)
    else tree
  }

  private def transform(tree: Apply)(implicit ctx: Context): Block = {
    val Apply(Apply(_, elems), List(sem)) = tree
    val semanticSym = sem.tpe.widen.classSymbol.companionModule

    if (tree.symbol == AutoSeqApply) {
      val semantic = AutoSeqSemantics.apply(semanticSym)
      val tpParams = tree.tpe.baseArgInfos(IterableClass)

      val impl = semantic match {
        case Immutable => ImmutableSeqType
        case Mutable   => MutableSeqType
        case Lazy      => LazySeqType
      }

      val anon = AnonClass(impl.appliedTo(tpParams) :: Nil, Nil, Nil)
      instances += anon -> new AutoSeq(anon.tpe.classSymbol, semantic, tpParams, elems)

      anon
    }

    else if (tree.symbol == AutoMapApply) {
      val semantic = AutoMapSemantics.apply(semanticSym)
      val tpParams = semantic match {
        case Lazy =>
          val List(tuple) = tree.tpe.baseArgInfos(IterableClass)
          val tuple2 = ctx.definitions.TupleType(2).classSymbol
          tuple.baseArgInfos(tuple2)
        case _ =>
          tree.tpe.baseArgInfos(MapClass)
      }

      val impl = semantic match {
        case Immutable => ImmutableMapType
        case Mutable   => MutableMapType
        case Lazy      => LazyMapType
      }

      val anon = AnonClass(impl.appliedTo(tpParams) :: Nil, Nil, Nil)
      instances += anon -> new AutoMap(anon.tpe.classSymbol, semantic, tpParams, elems)

      anon
    }

    else /* tree.symbol == AutoSetApply */ {
      val semantic = AutoSetSemantics.apply(semanticSym)
      val tpParams = tree.tpe.baseArgInfos(IterableClass)

      val impl = semantic match {
        case Immutable => ImmutableSetType
        case Mutable   => MutableSetType
        case Lazy      => LazySetType
      }

      val anon = AnonClass(impl.appliedTo(tpParams) :: Nil, Nil, Nil)
      instances += anon -> new AutoSet(anon.tpe.classSymbol, semantic, tpParams, elems)

      anon
    }
  }

}

object PreAutoCollections {

  private def autoCollectionApply(collection: String)(implicit ctx: Context) =
    ctx.requiredModule(s"scala.collection.AutoCollections.$collection").requiredMethod(nme.apply)

  private def autoCollectionType(collection: String)(implicit ctx: Context) =
    ctx.requiredClassRef(s"scala.collection.AutoCollections.$collection")

  private def autoCollectionSemantics(semantic: Semantic => Symbol)(implicit ctx: Context) = Map(
    semantic(Immutable) -> Immutable,
    semantic(Mutable)   -> Mutable,
    semantic(Lazy)      -> Lazy
  )


  private def IterableClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Iterable")

  // ------------ Seq ------------
  private def SeqClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Seq")

  private def AutoSeqApply(implicit ctx: Context) = autoCollectionApply("AutoSeq")

  private def ImmutableSeqType(implicit ctx: Context) = autoCollectionType("ImmutableSeq")
  private def MutableSeqType(implicit ctx: Context)   = autoCollectionType("MutableSeq")
  private def LazySeqType(implicit ctx: Context)      = autoCollectionType("LazySeq")

  private def AutoSeqSemantics(implicit ctx: Context) = autoCollectionSemantics(sem =>
    ctx.requiredModule(s"scala.collection.AutoCollections.AutoSeq.$sem"))


  // ------------ Map ------------
  private def MapClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Map")

  private def AutoMapApply(implicit ctx: Context) = autoCollectionApply("AutoMap")

  private def ImmutableMapType(implicit ctx: Context) = autoCollectionType("ImmutableMap")
  private def MutableMapType(implicit ctx: Context)   = autoCollectionType("MutableMap")
  private def LazyMapType(implicit ctx: Context)      = autoCollectionType("LazyMap")

  private def AutoMapSemantics(implicit ctx: Context) = autoCollectionSemantics(sem =>
    ctx.requiredModule(s"scala.collection.AutoCollections.AutoMap.$sem"))


  // ------------ Set ------------
  private def SetClass(implicit ctx: Context) = ctx.requiredClass("scala.collection.Set")

  private def AutoSetApply(implicit ctx: Context) = autoCollectionApply("AutoSet")

  private def ImmutableSetType(implicit ctx: Context) = autoCollectionType("ImmutableSet")
  private def MutableSetType(implicit ctx: Context)   = autoCollectionType("MutableSet")
  private def LazySetType(implicit ctx: Context)      = autoCollectionType("LazySet")

  private def AutoSetSemantics(implicit ctx: Context) = autoCollectionSemantics(sem =>
    ctx.requiredModule(s"scala.collection.AutoCollections.AutoSet.$sem"))

}
