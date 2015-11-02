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

  def isIdempotentRef(tree: Tree)(implicit ctx: Context): Boolean =
    if (!tree.tpe.widen.isParameterless) true
    else if (tree.symbol hasAnnotation defn.IdempotentAnnotationClass) true
    else if (!tree.symbol.isStable) false
    else if (tree.symbol is Flags.Lazy) true
    else true

  def isIdempotentExpr(tree: Tree)(implicit ctx: Context): Boolean = tree match {
    case EmptyTree | This(_) | Super(_, _) | Literal(_) =>
      true
    case Ident(_) =>
      isIdempotentRef(tree)
    case Select(qual, _) =>
      isIdempotentExpr(qual) && isIdempotentRef(tree)
    case TypeApply(fn, _) =>
      isIdempotentExpr(fn)
    case Apply(fn, args) =>
      isIdempotentExpr(fn) && (args forall isIdempotentExpr)
    case Typed(expr, _) =>
      isIdempotentExpr(expr)
    case Block(stats, expr) =>
      isIdempotentExpr(expr) && (stats forall isIdempotentExpr)
    case _ =>
      false
  }

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
        if (isStableRef(qual.symbol))
          Some(Idempotent(tree.symbol, qual.symbol))
        else None

      // fun(args)
      case Apply(id: Ident, args) if isIdempotent(id.symbol) =>
        val argsSyms = args map (_.symbol)
        if (argsSyms forall isStableRef)
          Some(Idempotent(id.symbol, args = argsSyms))
        else None

      // qual.fun(args)
      case Apply(fun@Select(qual, _), args) if isIdempotent(fun.symbol) =>
        val argsSyms = args map (_.symbol)
        if ((qual.symbol :: argsSyms) forall isStableRef)
          Some(Idempotent(fun.symbol, qual.symbol, argsSyms))
        else None

      case _ =>
        None
    }

    def isStableRef(sym: Symbol)(implicit ctx: Context): Boolean =
      if (sym.isClass) true
      else (sym != NoSymbol) && (sym.owner == ctx.owner) && !(sym is Flags.Mutable)

    /** @return Return <code>true</code> if <code>sym</code> references an
      *          idempotent operation, <code>false</code> otherwise
      */
    def isIdempotent(sym: Symbol)(implicit ctx: Context): Boolean =
      (sym hasAnnotation defn.IdempotentAnnotationClass) || (sym is Flags.Lazy)
  }


  /** Transformation on trees which remove redundant idempotent calls */
  class IdempotentCallElimination extends TreeMap {

    /** Substitutions defined on idempotent calls */
    private var substs = Map.empty[Idempotent, Symbol]

    /** Buffer of extracted idempotent calls */
    private var treeBuffer = List.empty[ListBuffer[Tree]]

    /** Whether or not idempotent calls can be extracted in the current context */
    private var octx: OptimizationContext = notOptimizable

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      // Recursively call on subtrees
      val transformed = tree match {
        case Apply(id: Ident, args) if octx.optimizable =>
          val targs = transformArgs(args)
          cpy.Apply(tree)(id, targs)

        case Apply(sel@Select(qual, name), args) if octx.optimizable =>
          val tqual :: targs = transformArgs(qual :: args)
          val cpSel = cpy.Select(sel)(tqual, name)
          cpy.Apply(tree)(cpSel, targs)

        case Block(stats, expr) =>
          val savedSubsts = substs
          val savedCtx = octx
          octx = optimizable

          val tStats = transformStats(stats)

          treeBuffer ::= ListBuffer.empty
          val tExpr = transform(expr)
          val vds = treeBuffer.head.toList
          treeBuffer = treeBuffer.tail

          substs = savedSubsts
          octx = savedCtx
          cpy.Block(tree)(tStats ::: vds, tExpr)

        case dd: DefDef =>
          super.transform(tree)(ctx withOwner dd.symbol)

        case _ =>
          super.transform(tree)
      }

      // Apply a substitution if defined,
      // try to extract an idempotent call otherwise
      (Idempotent(transformed) fold transformed) { idem =>
        substs get idem match {
          // New idempotent call in optimizable context
          case None if octx.optimizable =>
            val holderName = ctx.freshName().toTermName
            val valDef = SyntheticValDef(holderName, transformed)
            substs += idem -> valDef.symbol
            treeBuffer.head += valDef
            ref(valDef.symbol)

          // Redundant idempotent call
          case Some(symb) => ref(symb)
          case _          => tree
        }
      }
    }

    override def transformStats(trees: List[Tree])(implicit ctx: Context): List[Tree] = {
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

    /** Try to extract idempotent calls in <code>args</code> until
      *  we encounter a not idempotent expression
      */
    def transformArgs(args: List[Tree])(implicit ctx: Context) = {
      val (idems, rest) = args span isIdempotentExpr
      val tidems = transform(idems)
      val saved = octx
      octx = notOptimizable
      val trest = transform(rest)
      octx = saved
      tidems ::: trest
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
