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
    val transformer = new IdempotentCallElimination(ctx.owner)
    val result = transformer.transform(tree)
    println("-" * 50)
    println(result.show + "\n\n")
    result
  }

  def isIdempotentRef(tree: Tree)(implicit ctx: Context): Boolean =
    if (!tree.tpe.widen.isParameterless) true
    else if (tree.symbol hasAnnotation defn.IdempotentAnnot) true
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
    * @param fun method symbol
    * @param qualifier optional qualifier symbol
    * @param args optional list of argument symbol
    */
  case class Idempotent(fun: Symbol, qualifier: Symbol = NoSymbol, args: List[Symbol] = Nil)

  object Idempotent {

    /** Try to extract an idempotent call from <code>tree</code>
      *
      * @return The idempotent call as an option, <code>None</code>
      *          if no idempotent call could be extracted
      */
    def apply(tree: Tree)(implicit ctx: Context): Option[Idempotent] = tree match {
      // fun
      case Ident(_) if isExtractable(tree.symbol) =>
        Some(Idempotent(tree.symbol))

      // qual.fun
      case Select(qual, _) if isExtractable(tree.symbol) =>
        if (isStableRef(qual.symbol))
          Some(Idempotent(tree.symbol, qual.symbol))
        else None

      // fun(args)
      case Apply(id: Ident, args) if isExtractable(id.symbol) =>
        val argsSyms = args map (_.symbol)
        if (argsSyms forall isStableRef)
          Some(Idempotent(id.symbol, args = argsSyms))
        else None

      // qual.fun(args)
      case Apply(fun @ Select(qual, _), args) if isExtractable(fun.symbol) =>
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
      *         extractable idempotent operation, <code>false</code> otherwise
      */
    def isExtractable(sym: Symbol)(implicit ctx: Context): Boolean =
      (sym hasAnnotation defn.IdempotentAnnot) || (sym is Flags.Lazy)
  }


  /** Transformation on trees which remove redundant idempotent calls */
  class IdempotentCallElimination(_owner: Symbol) extends TreeMap {

    /** Enclosing owner */
    private var owner = _owner

    /** Substitutions defined on idempotent calls */
    private var substs = Map.empty[Idempotent, Symbol]

    /** Buffers of extracted idempotent calls */
    private var treeBuffers = List.empty[ListBuffer[Tree]]

    /** Whether or not idempotent calls can be extracted in the current context */
    private var octx: OptimizationContext = notOptimizable


    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      // Recursively call on subtrees
      val transformed = tree match {
        case Apply(id: Ident, args) if octx.optimizable =>
          val targs = transformOpt(args)
          cpy.Apply(tree)(id, targs)

        case Apply(fun@Select(qual, name), args) if octx.optimizable =>
          val tqual :: targs = transformOpt(qual :: args)
          val cpSel = cpy.Select(fun)(tqual, name)
          cpy.Apply(tree)(cpSel, targs)

        case Block(stats, expr) =>
          withContext(optimizable) {
            // Save substitutions to avoid corruption by inner block
            val saved = substs
            val tStats = transformStats(stats)(ctx withOwner owner)

            treeBuffers ::= ListBuffer.empty
            val tExpr = transform(expr)(ctx withOwner owner)
            val vds = treeBuffers.head.toList
            treeBuffers = treeBuffers.tail

            substs = saved
            cpy.Block(tree)(tStats ::: vds, tExpr)
          }

        case If(cond, thenp, elsep) =>
          val List(tcond, tthenp, telsep) = transformOpt(List(cond, thenp, elsep))
          cpy.If(tree)(tcond, tthenp, telsep)

        case dd @ DefDef(name, tparams, vparamss, tpt, _) =>
          // Must be in a block
          withContext(notOptimizable) {
            val rhs = withOwner(dd.symbol)(transform(dd.rhs))
            cpy.DefDef(dd)(name, transformSub(tparams), vparamss mapConserve (transformSub(_)), transform(tpt), rhs)
          }

        case vd @ ValDef(name, _tpt, _) =>
          val tpt = transform(_tpt)
          val rhs = withOwner(vd.symbol)(transform(vd.rhs))
          cpy.ValDef(vd)(name, tpt, rhs)

        case td: TypeDef => td
        case tt: TypTree => tt
        case _           => super.transform(tree)
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
            treeBuffers.head += valDef
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
        treeBuffers ::= ListBuffer.empty
        val tstat = transform(stat)
        treeBuffers.head += tstat
        val res = treeBuffers.head.toList
        treeBuffers = treeBuffers.tail
        res
      }
    }

    /** Transform <code>trees</code> under the current optimisation
      * context unitil we hit a not idempotent tree. Transform the
      * rest under a not optimizable context.
      */
    private def transformOpt(trees: List[Tree])(implicit ctx: Context) = {
      withSavedContext {
        trees mapConserve { t =>
          if (octx.optimizable && !isIdempotentExpr(t)) {
            octx = notOptimizable
          }
          transform(t)
        }
      }
    }

    /** Evaluate <code>expr</code> with the optimisation context <code>noctx</code> */
    private def withContext[T](noctx: OptimizationContext)(expr: => T) = {
      withSavedContext {
        octx = noctx
        expr
      }
    }

    /** Evaluate <code>expr</code> saving the current context */
    private def withSavedContext[T](expr: => T) = {
      val saved = octx
      val result = expr
      octx = saved
      result
    }

    /** Evaluate <code>expr</code> with the owner <code>nowner</code> */
    private def withOwner[T](nowner: Symbol)(expr: => T) = {
      val saved = owner
      owner = nowner
      val result = expr
      owner = saved
      result
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
