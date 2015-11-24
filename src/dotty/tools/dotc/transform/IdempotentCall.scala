package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.mutable.ListBuffer

class IdempotentCall extends MiniPhaseTransform {

  import IdempotentCall._
  import tpd._

  override def phaseName: String = "idempotentCall"

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
//    println(tree.show)
    val transformer = new IdempotentCallElimination(ctx.owner)
    val result = transformer.transform(tree)
//    println("-" * 50)
//    println(result.show + "\n\n")
    result
  }

  def isIdempotentRef(tree: Tree)(implicit ctx: Context): Boolean =
    if (!tree.tpe.widen.isParameterless) true
    else if (tree.symbol hasAnnotation defn.IdempotentAnnot) true
    else if (!tree.symbol.isStable) false
    else if (tree.symbol is Lazy) true
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
  case class Idempotent(fun: Symbol, qualifier: Symbol = NoSymbol, args: List[Type] = Nil)(implicit ctx: Context) {
    override def equals(that: Any): Boolean = that match {
      case idem @ Idempotent(thatFun, thatQualifier, thatArgs) =>
        (this eq idem) || {
          fun == thatFun && qualifier == thatQualifier &&
            args.size == thatArgs.size && (args zip thatArgs).forall(t => t._1 =:= t._2)
        }
      case _ =>
        false
    }

    override def hashCode: Int =
      fun.hashCode + qualifier.hashCode
  }

  object Idempotent {

    /** Try to extract an idempotent call from <code>tree</code>
      *
      * @return The idempotent call as an option, <code>None</code>
      *         if no idempotent call could be extracted
      */
    def apply(tree: Tree)(implicit ctx: Context): Option[Idempotent] = tree match {
      // fun
      case Ident(_) if isExtractable(tree.symbol) =>
        Some(Idempotent(tree.symbol))

      // qual.fun
      case Select(qual, _) if isExtractable(tree.symbol) =>
        if (isImmutableRef(qual))
          Some(Idempotent(tree.symbol, qual.symbol))
        else None

      // fun(args)
      case Apply(id: Ident, args) if isExtractable(id.symbol) =>
        if (args forall isImmutableRef)
          Some(Idempotent(id.symbol, args = args map (_.tpe)))
        else None

      // qual.fun(args)
      case Apply(fun @ Select(qual, _), args) if isExtractable(fun.symbol) =>
        if ((qual :: args) forall isImmutableRef)
          Some(Idempotent(fun.symbol, qual.symbol, args map (_.tpe)))
        else None

      case _ =>
        None
    }

    /** @return <code>true</code> if <code>tree</code> is:
      *          - a constant
      *          - <code>val</code>
      *          - <code>this</code>
      *          - <code>super</code>
      *         <code>false</code> otherwise
      */
    private def isImmutableRef(tree: Tree)(implicit ctx : Context): Boolean = tree.tpe match {
      case tr: TermRef     => !(tr.symbol is Mutable) && !(tr.symbol is Method)
      case _: ThisType     => true
      case _: SuperType    => true
      case _: ConstantType => true
      case _: RefinedThis  => true
      case _               => false
    }

    /** @return Return <code>true</code> if <code>sym</code> references an
      *         extractable idempotent operation, <code>false</code> otherwise
      */
    private def isExtractable(sym: Symbol)(implicit ctx: Context): Boolean =
      if (sym hasAnnotation defn.IdempotentAnnot) true
      else (sym is Lazy) && !(sym is JavaDefined) // lazy val and singleton objects
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
          val tcond = transform(cond)
          withContext(notOptimizable) {
            cpy.If(tree)(tcond, transform(thenp), transform(elsep))
          }


        case dd @ DefDef(name, tparams, vparamss, tpt, _) =>
          // Must be in a block
          withContext(notOptimizable) {
            val rhs = withOwner(dd.symbol)(transform(dd.rhs))
            cpy.DefDef(dd)(name, transformSub(tparams), vparamss mapConserve (transformSub(_)), transform(tpt), rhs)
          }

        case vd @ ValDef(name, _tpt, _) =>
          val sym = vd.symbol
          val tpt = transform(_tpt)
          val rhs = withOwner(sym)(transform(vd.rhs))

          // Create a new val anyway if we have a var
          if (sym is Mutable) cpy.ValDef(vd)(name, tpt, rhs)
          else {
            treeBuffers.headOption flatMap (_.lastOption) match {
              // Newly created val is a duplicate of the current one
              case Some(nvd: ValDef) if nvd.symbol == rhs.symbol && sym.info =:= nvd.tpe.widen =>
                treeBuffers.head -= nvd
                val idem = Idempotent(nvd.rhs).get
                substs += idem -> sym
                ValDef(sym.asTerm, nvd.rhs)

              case _ =>
                cpy.ValDef(vd)(name, tpt, rhs)
            }
          }

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
//            val valDef = SyntheticValDef(holderName, transformed)
            val holderSym = ctx.newSymbol(ctx.owner, holderName, Synthetic, transformed.tpe.widenExpr, coord = transformed.pos)
            val valDef = ValDef(holderSym, transformed)
            substs += idem -> valDef.symbol
            treeBuffers.head += valDef
            ref(valDef.symbol)

          // Redundant idempotent call
          case Some(symb) =>
            assert(symb.info.widen =:= transformed.tpe.widen) // TODO: Remove
            ref(symb).ensureConforms(transformed.tpe)
          case _ =>
            tree
        }
      }
    }

    override def transformStats(trees: List[Tree])(implicit ctx: Context): List[Tree] = {
      trees flatMap { stat =>
        treeBuffers ::= ListBuffer.empty
        val tstat = transform(stat)
        // Prepend newly created vals to the transformed statement
        treeBuffers.head += tstat
        val res = treeBuffers.head.toList
        treeBuffers = treeBuffers.tail
        res
      }
    }

    /** Transform <code>trees</code> under the current optimisation
      * context until we hit a not idempotent tree. Transform the
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
