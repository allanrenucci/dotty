package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{Type, TypeRef}
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PreAutoCollections extends MiniPhaseTransform {

  import AutoCollections._
  import tpd._

  override def phaseName: String = "preautocollections"

  var anonClasses = Map.empty[Block, (List[Tree], List[Type])]


  private def AutoCollectionType(implicit ctx: Context): Map[Symbol, TypeRef] = Map(
    AutoSeqApply -> AutoSeqType,
    AutoMapApply -> AutoMapType,
    AutoSetApply -> AutoSetType
  )

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    AutoCollectionType.get(tree.symbol).fold(tree: Tree) { tpe =>
      val tpArgs = tree.tpe.argInfos
      val anonCls = AnonClass(tpe.appliedTo(tpArgs) :: Nil, Nil, Nil)

      anonClasses += anonCls -> (tree.args, tpArgs)
      anonCls
    }
  }

}

object PreAutoCollections {
  sealed trait Semantic
  case object Immutable extends Semantic
  case object Mutable extends Semantic
  case object Lazy extends Semantic


}
