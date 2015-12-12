package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.mutable.Buffer

class IdempotentCall extends MiniPhaseTransform {

  import IdempotentCall._
  import tpd._

  type Substitutions = Map[Idempotent, Symbol]

  override def phaseName: String = "idempotentCall"

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
//    println(tree.show)
    val result = transformDefDef(tree, Map.empty)
//    println("-" * 50)
//    println(result.show + "\n\n")
    result
  }

  private def transformDefDef(tree: DefDef, substs: Substitutions)(implicit ctx: Context): Tree = {
    val DefDef(name, tparams, vparamss, tpt, _) = tree
    val transformer = new IdempotentCallElimination(tree.symbol, substs)
    val rhs = transformer.transform(tree.rhs)(ctx withOwner tree.symbol)
    cpy.DefDef(tree)(name, tparams, vparamss, tpt, rhs)
  }

  /** Transformation on trees which remove redundant idempotent calls */
  class IdempotentCallElimination(_owner: Symbol, _subst: Substitutions) extends TreeMap {

    /** Enclosing block owner */
    private var blockOwner = _owner

    /** Substitutions defined on idempotent calls */
    private var substs = _subst

    /** Buffers of extracted idempotent calls */
    private var treeBuffers = List.empty[Buffer[Tree]]

    /** Whether or not idempotent calls can be extracted in the current context */
    private var octx: OptimizationContext = notOptimizable

    /** Substitutions available for inner functions */
    private var innerFuns: Map[Symbol, Set[Idempotent]] = Map.empty

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      def localCtx =
        if (tree.hasType && tree.symbol.exists) ctx withOwner tree.symbol else ctx

      tree match {
        // Function call
        case Apply(_, _) | TypeApply(_, _) | Select(_, _) | Ident(_) =>
          val sym = tree.symbol
          val isInnerFun = (sym is Method) && (sym.owner == ctx.owner)

          if (isInnerFun) {
            // Save substitions available at this method call
            innerFuns += sym -> (substs.keySet ++ innerFuns.getOrElse(sym, Set.empty))
          }

          transformMethodCall(tree)

        case Block(stats, expr) =>
          def transformStat(stat: Tree): (Buffer[Tree], Tree) = {
            treeBuffers ::= Buffer.empty
            val tstat = transform(stat)
            val extracted :: tail = treeBuffers
            treeBuffers = tail
            (extracted, tstat)
          }

          withContext(optimizable) {
            // Save state of optimisation. It will be corrupted by inner transformations
            val savedSubsts = substs
            val savedBlockOwner = blockOwner

            blockOwner = ctx.owner

            // transform statements and prepend extracted calls
            val tStats = stats flatMap { stat =>
              val (extracted, tstat) = transformStat(stat)
              extracted += tstat
            }

            val (extracted, tExpr) = transformStat(expr)

            // Restore state
            substs = savedSubsts
            blockOwner = savedBlockOwner

            cpy.Block(tree)(tStats ++ extracted, tExpr)
          }

        case If(cond, thenp, elsep) =>
          val tcond = transform(cond)
          withContext(notOptimizable) {
            cpy.If(tree)(tcond, transform(thenp), transform(elsep))
          }

        case dd: DefDef =>
          // If the method call is before the method declaration, we should not
          // use the current substitutions but the ones defined at the call place.
          val availableSubsts = (innerFuns get dd.symbol fold substs) { idems =>
            idems.map(i => i -> substs(i)).toMap
          }
          transformDefDef(dd, availableSubsts)

        case vd @ ValDef(name, _tpt, _) =>
          implicit val ctx: Context = localCtx

          val sym = vd.symbol
          val tpt = transform(_tpt)
          val rhs = transform(vd.rhs)
          lazy val copy = cpy.ValDef(vd)(name, tpt, rhs)

          // Do not extract call if it is the rhs of a value definition:
          // val a = idem() -/-> val $1$ = idem(); val a = $1$
          if (sym is Mutable) copy
          else {
            treeBuffers.headOption flatMap (_.lastOption) collect {
              case nvd: ValDef if nvd.symbol == rhs.symbol && sym.info =:= nvd.tpe.widen =>
                treeBuffers.head -= nvd
                val idem = substs.find(_._2 == nvd.symbol).get._1
                substs += idem -> sym
                ValDef(sym.asTerm, nvd.rhs)
            } getOrElse copy
          }

        case temp: Template =>
          // Don't optimize body of a class (could create not wanted fields)
          withContext(notOptimizable) {
            super.transform(temp)
          }

        case td: TypeDef => td
        case tt: TypTree => tt
        case _           => super.transform(tree)
      }
    }

    private def transformMethodCall(tree: Tree)(implicit ctx: Context): Tree = {
      def transformIdem(t: Tree): Tree = {
        if (octx.optimizable && !isIdempotentExpr(t)) {
          octx = notOptimizable
        }
        transform(t)
      }

      var qualifier: Tree = EmptyTree
      var arguments = List.empty[List[Tree]]
      var typeArguments = List.empty[Tree]

      // Recursively transform inner trees and collect qualifier, arguments and type arguments
      def innerTransform(t: Tree): Tree = {
        assert(t.symbol == tree.symbol) // TODO: Remove
        t match {
          case Apply(fun, args) =>
            val tfun = innerTransform(fun)
            val targs = args mapConserve transformIdem
            arguments ::= targs
            cpy.Apply(t)(tfun, targs)

          case TypeApply(fun, args) =>
            val tfun = innerTransform(fun)
            typeArguments = transform(args)
            cpy.TypeApply(t)(tfun, typeArguments)

          case Select(qual, name) =>
            qualifier = transformIdem(qual)
            cpy.Select(t)(qualifier, name)

          case Ident(_) =>
            t
        }
      }

      val transformed = withSavedContext {
        innerTransform(tree)
      }
      // Is this call an extractable idempotent call
      val idemOption = Idempotent.from(qualifier, tree.symbol, typeArguments, arguments)

      (idemOption fold transformed) { idem =>
        substs get idem match {
          // New idempotent call in optimizable context
          case None if octx.optimizable =>
            val holderName = ctx.freshName().toTermName
            val holderSym = ctx.newSymbol(blockOwner, holderName, Synthetic, transformed.tpe.widenExpr, coord = transformed.pos)
            val valDef = ValDef(holderSym, transformed)
            substs += idem -> valDef.symbol
            treeBuffers.head += valDef
            val result = ref(valDef.symbol)
            result

          // Redundant idempotent call
          case Some(symb) =>
            assert(symb.info.widen =:= tree.tpe.widen) // TODO: Remove
            ref(symb).ensureConforms(tree.tpe)

          case _ =>
            transformed
        }
      }
    }

    /** Evaluate `expr` with the optimisation context `noctx` */
    private def withContext[T](noctx: OptimizationContext)(expr: => T) = {
      withSavedContext {
        octx = noctx
        expr
      }
    }

    /** Evaluate `expr` saving the current context */
    private def withSavedContext[T](expr: => T) = {
      val saved = octx
      val result = expr
      octx = saved
      result
    }
  }

  private def isIdempotentExpr(tree: Tree)(implicit ctx: Context): Boolean = {
    def isIdempotentRef(tree: Tree): Boolean = {
      if (!tree.tpe.widen.isParameterless) true
      else if (tree.symbol hasAnnotation defn.IdempotentAnnot) true
      else if (!tree.symbol.isStable) false
      else if (tree.symbol is Lazy) true
      else true
    }

    tree match {
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
  }

}

object IdempotentCall {

  import tpd._

  /** Idempotent call representation
    *
    * @param fun method symbol
    * @param qual optional qualifier symbol
    * @param typeParams type parameters
    * @param args arguments types
    * @param ctx context
    */
  case class Idempotent(fun: Symbol,
                        qual: Symbol,
                        typeParams: List[Type],
                        args: List[List[Type]])(implicit ctx: Context) {

    override def equals(that: Any): Boolean = that match {
      case that: Idempotent =>
        if (this eq that) true
        else if (fun != that.fun) false
        else if (qual != that.qual) false
        else if (typeParams.size != that.typeParams.size) false
        else if (typeParams zip that.typeParams exists (t => !(t._1 =:= t._2))) false
        else if (args.size != that.args.size) false
        else (args zip that.args) forall {
          case (a1s, a2s) =>
            a1s.size == a2s.size && (a1s zip a2s).forall(t => t._1 =:= t._2)
        }

      case _ =>
        false
    }

    override def hashCode: Int =
      fun.hashCode + qual.hashCode
  }

  object Idempotent {

    def from(qual: Tree, fun: Symbol, tpeArgs: List[Tree], args: List[List[Tree]])(implicit ctx: Context):
        Option[Idempotent] = {

      /** @return `true` if `tree` is:
        *          - a constant
        *          - `val`
        *          - `this`
        *          - `super`
        *         `false` otherwise
        */
      def isImmutableRef(tree: Tree) = tree.tpe match {
        case tr: TermRef     => !(tr.symbol is Mutable) && !(tr.symbol is Method)
        case _: ThisType     => true
        case _: SuperType    => true
        case _: ConstantType => true
        case _: RefinedThis  => true
        case _               => false
      }

      /** @return Return `true` if `sym` references an
        *         extractable idempotent operation, `false` otherwise
        */
      def isExtractable(sym: Symbol) = {
        if (sym hasAnnotation defn.IdempotentAnnot) true
        else (sym is Lazy) && !(sym is JavaDefined) // lazy val and singleton objects
      }

      val isIdem = isExtractable(fun) && (qual == EmptyTree || isImmutableRef(qual)) &&
        (args forall (_ forall isImmutableRef))

      if (isIdem) {
        val argsTpe = args map (_ map (_.tpe))
        Some(Idempotent(fun, qual.symbol, tpeArgs map (_.tpe), argsTpe))
      }
      else None
    }

  }

  final class OptimizationContext(val optimizable: Boolean) extends AnyVal

  /** Apply substitutions and try to extract idempotent calls */
  final val optimizable = new OptimizationContext(true)

  /** Only apply substitutions */
  final val notOptimizable = new OptimizationContext(false)
}
