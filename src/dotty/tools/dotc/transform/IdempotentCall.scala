package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}
import dotty.tools.dotc.core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class IdempotentCall extends MiniPhaseTransform {

  import tpd._

  override def phaseName: String = "IdempotentCall"

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    println(tree.show)
    val transformer = new IdempotentCallElimination
    val result = transformer.transform(tree)(ctx withOwner tree.symbol)
    println("-" * 50)
    println(result.show + "\n\n")
    result
  }

  /** Extract idempotent calls in <code>tree</code>
   *  - lazy val
   *  - methods annotated with <code>@Idempotent</code>
   */
  private def extractIdempotentCall(tree: Tree)(implicit ctx: Context): List[Tree] = tree match {
    case vd: ValDef => extractIdempotentCall(vd.rhs)
    case expr if tpd isIdempotentExpr expr=>
      expr match {
        // local lazy val or parameterless method on singleton object
        case _: Ident if tree.symbol is Flags.MethodOrLazy =>
          List(tree)

        // method on singleton object
        case Apply(_: Ident, args) =>
          List(tree)

        // parameterless method/field on this instance
        case Select(_: This, _) if expr.symbol is Flags.MethodOrLazy =>
          List(tree)

        // parameterless method/field on object instance
        case Select(_: Ident, _) if expr.symbol is Flags.MethodOrLazy =>
          List(tree)

        // method on this instance
        case Apply(Select(_: This, _), args) =>
          List(tree)

        // method on object instance
        case Apply(Select(_: Ident, _), args) =>
          List(tree)

        case _ => Nil

      }
    case _ => Nil

  }

  /** Idempotent call representation
   *
   *  @param fun method symbol
   *  @param qualifier optional qualifier symbol
   *  @param args optional list of argument symbol
   */
  case class Idempotent(fun: Symbol, qualifier: Symbol = NoSymbol, args: List[Symbol] = Nil)

  /** Transformation on trees which remove redundant idempotent calls */
  class IdempotentCallElimination extends TreeMap {
    type State = Map[Idempotent, Symbol]

    // Available idempotent calls
    private var state: State = Map.empty[Idempotent, Symbol]

    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case Ident(_) =>
        val idem = Idempotent(tree.symbol)
        (state get idem fold tree) (ref)

      case sel@Select(This(_) | Ident(_), _) =>
        val idem = Idempotent(tree.symbol, sel.qualifier.symbol)
        (state get idem fold tree) (ref)

      case Block(stats, expr) =>
        val beforeTransform = state
        val tStats = transformBlockStats(stats)
        val tExpr = transform(expr)
        state = beforeTransform // reset state to its value before transforming statements
        cpy.Block(tree)(tStats, tExpr)

      //      case Apply(s @ Select(qualifier, name), args) =>
      //        println(s.symbol)
      //        println(qualifier.symbol + "\n")
      //        tree
      //
      //      case Apply(id: Ident, args) =>
      //        println(id.symbol + "\n")
      //        tree

      case _ =>
        super.transform(tree)
    }

    /** Traverse <code>trees</code> applying transformation, extracting
     *  idempotent calls, and augmenting <code>state</code> with the
     *  extracted calls and their substitution.
     */
    def transformBlockStats(trees: List[Tree])(implicit ctx: Context): List[Tree] = {
      def inner(trees: List[Tree]): List[Tree] = trees match {
        case s :: ss =>
          val trans = transform(s)
          val idemCalls = extractIdempotentCall(trans)

          val vds = idemCalls map { tree =>
            val holderName = ctx.freshName().toTermName
            val valDef = SyntheticValDef(holderName, tree)

            state += (tree match {
              case id: Ident =>
                Idempotent(id.symbol)
              case Apply(id: Ident, args) =>
                Idempotent(id.symbol, NoSymbol, ???)
              case sel@Select(th: This, _) =>
                Idempotent(sel.symbol, th.symbol)
              case sel@Select(id: Ident, _) =>
                Idempotent(sel.symbol, id.symbol)
              case Apply(sel@Select(th: This, _), args) =>
                Idempotent(sel.symbol, th.symbol, ???)
              case Apply(sel@Select(id: Ident, _), args) =>
                Idempotent(sel.symbol, id.symbol, ???)
            }) -> valDef.symbol

            valDef
          }

          // Reapply transformation with augmented state
          vds ::: (transform(trans) :: inner(ss))

        case _ =>
          Nil
      }

      flatten(inner(trees))
    }

  }

}
