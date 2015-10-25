package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.mutable.ListBuffer

class IdempotentCall extends MiniPhaseTransform {

  import IdempotentCall._
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

  /** @return Return <code>true</code> if <code>sym</code> references an
    *          idempotent operation, <code>false</code> otherwise
    */
  def isIdempotent(sym: Symbol)(implicit ctx: Context): Boolean =
    ((sym is Flags.Method) && (sym hasAnnotation defn.IdempotentAnnotationClass)) || (sym is Flags.Lazy)

  /** Idempotent call representation
    *
    *  @param fun method symbol
    *  @param qualifier optional qualifier symbol
    *  @param args optional list of argument symbol
    */
  case class Idempotent(fun: Symbol, qualifier: Symbol = NoSymbol, args: List[Symbol] = Nil)

  object Idempotent {

    /** Try to extract an idempotent call from <code>tree</code>
      *
      *  @return The idempotent call as an option, <code>None</code>
      *          if no idempotent call could be extracted
      */
    def apply(tree: Tree)(implicit ctx: Context): Option[Idempotent] = tree match {
      // fun
      case Ident(_) if isIdempotent(tree.symbol) =>
        Some(Idempotent(tree.symbol))

      // qual.fun
      case Select(qual, _) if isIdempotent(tree.symbol) =>
        if (qual.symbol == NoSymbol) None
        else Some(Idempotent(tree.symbol, qual.symbol))

      // fun(args)
      case Apply(id: Ident, args) if isIdempotent(id.symbol) =>
        val argsSyms = args map (_.symbol)
        if (argsSyms contains NoSymbol) None
        else Some(Idempotent(id.symbol, args = argsSyms))

      // qual.fun(args)
      case Apply(fun@Select(qual, _), args) if isIdempotent(fun.symbol) =>
        val argsSyms = args map (_.symbol)
        if ((qual.symbol :: argsSyms) contains NoSymbol) None
        else Some(Idempotent(fun.symbol, qual.symbol, argsSyms))

      case _ =>
        None
    }
  }


  /** Transformation on trees which remove redundant idempotent calls */
  class IdempotentCallElimination extends TreeMap {

    /** Substitutions defined on idempotent calls */
    private var substs = Map.empty[Idempotent, Symbol]

    /** Buffer of extracted idempotent calls */
    private var treeBuffer = List.empty[ListBuffer[Tree]]

    /** Whether or not idempotent calls can be extracted in the current context */
    private var ctx: OptimizationContext = notOptimizable

    override def transform(tree: Tree)(implicit c: Context): Tree = {
      val trans = transformIdem(tree)
      // extract idempotent call in subtrees
      trans match {
        case Select(qual, name) if ctx.optimizable =>
          transformIdem(Select(transform(qual), name))

        case Apply(id: Ident, args) if ctx.optimizable =>
          val (pures, rest) = transformArgs(args)
          transformIdem(Apply(id, pures ::: rest))

        case Apply(Select(qual, name), args) if ctx.optimizable =>
          val tqual = transform(qual)
          val (pures, rest) = transformArgs(args)
          transformIdem(Apply(Select(tqual, name), pures ::: rest))

        case Block(stats, expr) =>
          val savedSubsts = substs
          val savedCtx = ctx
          ctx = optimizable

          val tStats = transformStats(stats)

          treeBuffer ::= ListBuffer.empty
          val tExpr = transform(expr)
          val vds = treeBuffer.head.toList
          treeBuffer = treeBuffer.tail

          substs = savedSubsts
          ctx = savedCtx
          transformIdem(Block(tStats ::: vds, tExpr))

        case _ =>
          super.transform(trans)
      }
    }

    override def transformStats(trees: List[Tree])(implicit c: Context): List[Tree] = {
      trees flatMap { stat =>
        // Prepend newly created vals to the transformed statement
        treeBuffer ::= ListBuffer.empty
        val tstat = transform(stat)
        treeBuffer.head += tstat
        val res = treeBuffer.head.toList
        treeBuffer = treeBuffer.tail
        res
      }
    }

    /** If defined, apply a substitution on <code>tree</code>,
      *  try to extract an idempotent call otherwise
      */
    def transformIdem(tree: Tree)(implicit c: Context): Tree =
      (Idempotent(tree) fold tree) { idem =>
        substs get idem match {
          // New idempotent call in optimizable context
          case None if ctx.optimizable =>
            val holderName = c.freshName().toTermName
            val valDef = SyntheticValDef(holderName, tree)
            substs += idem -> valDef.symbol
            treeBuffer.head += valDef
            ref(valDef.symbol)

          // Redundant idempotent call
          case Some(symb) => ref(symb)
          case _          => tree
        }
      }

    /** Try to extract idempotent calls in <code>args</code> until
      *  we encounter an impure expression
      */
    def transformArgs(args: List[Tree])(implicit c: Context) = {
      val (pures, rest) = args span isPureExpr
      val tpures = transform(pures)
      val saved = ctx
      ctx = notOptimizable
      val trest = transform(rest)
      ctx = saved
      (tpures, trest)
    }

  }

}

object IdempotentCall {

  final class OptimizationContext(val optimizable: Boolean) extends AnyVal

  /** Apply substitutions and try to extract idempotent calls */
  final val optimizable = new OptimizationContext(true)

  /** Only apply substitutions */
  final val notOptimizable = new OptimizationContext(false)
}
