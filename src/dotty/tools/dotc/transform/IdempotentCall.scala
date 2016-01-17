package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.mutable

/** A transformer which performs common subexpression elimination (CES)
  * on idempotent function calls.
  */
class IdempotentCall extends MiniPhaseTransform {

  import IdempotentCall._
  import tpd._

  override def phaseName: String = "idempotentCall"

  /** Should run after [[ElimByName]] to avoid idempotent calls extraction
    * for by name function parameters.
    * {{{
    *   lazy val idem = ???
    *
    *   Some(1) getOrElse idem
    *
    *   // Invalid idempotent calls extraction: changes the program behavior
    *   val $1$ = idem
    *   Some(1) getOrElse $1$
    * }}}
    */
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[ElimByName])

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    // Avoid transforming inner functions multiple times
    if (!tree.symbol.owner.isClass) tree
    else {
      val transformer = new IdempotentCallElimination
      val result = transformer.transform(tree)
//      println(tree.show)
//      println("-" * 50)
//      println(result.show + "\n\n")
      result
    }
  }

  /** Transformation on trees which remove duplicated idempotent calls */
  class IdempotentCallElimination extends TreeMap {

    /** Enclosing block owner */
    private var blockOwner: Symbol = NoSymbol

    /** Substitutions defined on idempotent calls */
    private var substs = Map.empty[Idempotent, Symbol]

    /** Whether or not idempotent calls can be extracted in the current context */
    private var octx = NotOptimizable

    /** Substitutions available for inner functions */
    private var innerFuns = Map.empty[Symbol, Set[(Idempotent, Symbol)]]

    /** Depth of the currently visited tree */
    private var currentDepth = 0

    /** Depth of extracted idempotent calls */
    private var depths = Map.empty[Symbol, Int]

    /** Buffers of extracted idempotent calls */
    private var treeBuffers = List.empty[mutable.Buffer[Tree]]


    /** Rewrite the tree to contain no duplicated idempotent calls */
    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      def localCtx =
        if (tree.hasType && tree.symbol.exists) ctx withOwner tree.symbol else ctx

      tree match {
        // Function call
        case Apply(_, _) | TypeApply(_, _) | Select(_, _) | Ident(_) =>
          transformMethodCall(tree)

        case Block(stats, expr) =>
          withSavedState {
            def transformStat(stat: Tree): (mutable.Buffer[Tree], Tree) = {
              treeBuffers ::= mutable.Buffer.empty
              val tstat = transform(stat)
              val extracted :: tail = treeBuffers
              treeBuffers = tail
              (extracted, tstat)
            }

            blockOwner = ctx.owner
            octx = Optimizable // We can safely extract new calls
            currentDepth += 1

            // We transform inner functions at the end in order to collect
            // available idempotent calls before transforming function bodies
            val tmpStats = stats flatMap {
              case defdef: DefDef => List(defdef)
              case stat =>
                val (extracted, tstat) = transformStat(stat)
                extracted += tstat // prepend extracted calls
            }

            val (extracted, tExpr) = transformStat(expr)

            val tStats = (tmpStats map {
              case defdef: DefDef => transform(defdef)
              case other          => other
            }) ++ extracted

            // Avoid creating new trees if block statements are left unchanged
            cpy.Block(tree)(if (tStats == stats) stats else tStats, tExpr)
          }

        case If(cond, thenp, elsep) =>
          val tcond = transform(cond)
          withSavedState {
            // We don't want to extract call in thenp and elsep unless in a block
            octx = NotOptimizable
            cpy.If(tree)(tcond, transform(thenp), transform(elsep))
          }

        case dd @ DefDef(name, tparams, vparamss, tpt, _) =>
          withSavedState {
            // Avoid extracting call in rhs which are not in a block
            // def foo = idem -/-> val $1$ = idem; def foo = $1$
            octx = NotOptimizable

            val funSym = dd.symbol

            // Filter out idempotent calls not reachable from the function body.
            // For instance, calls which are defined deeper than the function.
            val availableSubsts = innerFuns.getOrElse(funSym, Set.empty) filter {
              case (_, subst) =>
                depths(subst) <= currentDepth
            }

            substs = availableSubsts.toMap
            blockOwner = funSym

            val rhs = transform(dd.rhs)(ctx withOwner funSym)
            cpy.DefDef(tree)(name, tparams, vparamss, tpt, rhs)
          }

        case vd @ ValDef(name, tpt, _) =>
          implicit val ctx: Context = localCtx

          val sym = vd.symbol
          val rhs = transform(vd.rhs)
          lazy val copy = cpy.ValDef(vd)(name, tpt, rhs)

          // Do not extract call if it is the rhs of a value definition:
          // val a = idem() -/-> val $1$ = idem(); val a = $1$
          // Extract it anyway if `var` or rhs' type differs from value definition (e.g. val a: Double = 1)
          if (sym is Mutable) copy
          else {
            treeBuffers.headOption flatMap (_.lastOption) collect {
              case nvd: ValDef if nvd.symbol == rhs.symbol && sym.info =:= nvd.tpe.widen =>
                treeBuffers.head -= nvd
                val idem = substs.find(_._2 == nvd.symbol).get._1
                substs += idem -> sym
                depths += sym -> currentDepth
                ValDef(sym.asTerm, nvd.rhs)
            } getOrElse copy
          }

        case temp: Template =>
          withSavedState {
            // Don't optimize body of a class (could create not wanted fields)
            octx = NotOptimizable
            super.transform(temp)
          }

        case td: TypeDef => td
        case tt: TypTree => tt
        case _           => super.transform(tree)
      }
    }

    private def transformMethodCall(tree: Tree)(implicit ctx: Context): Tree = {

      /** Recursively transform and extract calls in qualifier and args.
        * If a subtree is not idempotent, subsequent trees cannot be extracted.
        */
      def transformSubTrees(t: Tree): Tree = {
        def transformAndUpdateContext(tree: Tree): Tree = {
          val result = transform(tree)
          if (octx.optimizable && !isIdempotent(result)) {
            octx = NotOptimizable
          }
          result
        }

        def inner(tree: Tree): Tree = tree match {
          case Apply(fun, args) =>
            val tfun = inner(fun)
            val targs = args mapConserve transformAndUpdateContext
            cpy.Apply(tree)(tfun, targs)

          case TypeApply(fun, args) =>
            val tfun = inner(fun)
            cpy.TypeApply(tree)(tfun, transform(args))

          case Select(qual, name) =>
            val tqual = transformAndUpdateContext(qual)
            cpy.Select(tree)(tqual, name)

          case Ident(_) =>
            tree
        }

        val savedOctx = octx
        val transformed = inner(tree)
        octx = savedOctx
        transformed
      }

      val transformed = transformSubTrees(tree)

      // To obtain the set of available substitutions of an inner function,
      // we perform the intersection of available substitutions between all
      // the call to the inner function.
      val sym = tree.symbol
      val isInnerFun = (sym is Method) && !sym.owner.isClass
      if (isInnerFun) {
        val substsSet = substs.toSet
        innerFuns += sym -> (innerFuns get sym fold substsSet)(_ intersect substsSet)
      }

      // Is this call an extractable idempotent call
      val idemOption = Idempotent.from(transformed)

      (idemOption fold transformed) { idem =>
        substs get idem match {
          // New idempotent call in optimizable context
          case None if octx.optimizable =>
            val holderName = ctx.freshName().toTermName
            // `tpe.widen` can be used instead if `ref(symb).ensureConforms(tree.tpe)`
            // is used when applying substitution
            val tpe = transformed.tpe.widenExpr
            val holderSym = ctx.newSymbol(blockOwner, holderName, Synthetic, tpe, coord = transformed.pos)
            val valDef = ValDef(holderSym, transformed)

            // Add a new substitution for this idempotent call
            substs += idem -> valDef.symbol
            depths += valDef.symbol -> currentDepth

            // Add the extracted value to a buffer to be inserted before the current statement
            treeBuffers.head += valDef
            ref(valDef.symbol)

          // Redundant idempotent call, apply substitution
          case Some(subst) =>
            //assert(subst.info.widen =:= tree.tpe.widen)
            ref(subst)//.ensureConforms(tree.tpe)

          case _ =>
            transformed
        }
      }
    }

    /** Evaluate `expr` saving the current optimisation state */
    private def withSavedState[T](expr: => T): T = {
      val savedOwner = blockOwner
      val savedSubsts = substs
      val savedOctx = octx
      val savedDepth = currentDepth

      val result = expr

      blockOwner = savedOwner
      substs = savedSubsts
      octx = savedOctx
      currentDepth = savedDepth

      result
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

    def from(tree: Tree)(implicit ctx: Context): Option[Idempotent] = {
      def isExtractable(tree: Tree) = {
        val sym = tree.symbol
        if (sym is Method) true
        else (sym is Lazy) && !(sym is JavaDefined)
      }

      def accumulate(tree: Tree, qual: Symbol, typeParams: List[Type], args: List[List[Type]]): Option[Idempotent] = {
        tree match {
          case Apply(fun, as) =>
            accumulate(fun, qual, typeParams, as.map(_.tpe) :: args)

          case TypeApply(fun, as) =>
            accumulate(fun, qual, as.map(_.tpe), args)

          case Select(qual, _) if isExtractable(tree) =>
            Some(Idempotent(tree.symbol, qual.symbol, typeParams, args))

          case Ident(_) if isExtractable(tree) =>
            Some(Idempotent(tree.symbol, qual, typeParams, args))

          case _ =>
            None
        }
      }

      if (isIdempotent(tree)) accumulate(tree, NoSymbol, Nil, Nil)
      else None
    }

  }

  private def isIdempotent(tree: Tree)(implicit ctx: Context): Boolean = {
    def isIdempotentRef(tree: Tree): Boolean = {
      val sym = tree.symbol
      if (sym hasAnnotation defn.IdempotentAnnot) true // @Idempotent methods
      else if (sym is Lazy) true                       // lazy val and singleton objects
      else !(sym is Mutable) && !(sym is Method)       // val
    }

    tree match {
      case EmptyTree | This(_) | Super(_, _) | Literal(_) =>
        true
      case Ident(_) =>
        isIdempotentRef(tree)
      case Select(qual, _) =>
        isIdempotent(qual) && isIdempotentRef(tree)
      case TypeApply(fn, _) =>
        isIdempotent(fn)
      case Apply(fn, args) =>
        isIdempotent(fn) && (args forall isIdempotent)
      case Typed(expr, _) =>
        isIdempotent(expr)
      case _ =>
        false
    }
  }

  final class OptimizationContext(val optimizable: Boolean) extends AnyVal

  /** Apply substitutions and try to extract idempotent calls */
  final val Optimizable = new OptimizationContext(true)

  /** Only apply substitutions */
  final val NotOptimizable = new OptimizationContext(false)
}
