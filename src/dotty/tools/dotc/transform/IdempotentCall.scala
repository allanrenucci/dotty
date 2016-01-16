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

  type Substitutions = Map[Idempotent, Symbol]

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
      //println(tree.show)
      //println("-" * 50)
      //println(result.show + "\n\n")
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
    private var octx = notOptimizable

    /** Substitutions available for inner functions */
    private var innerFuns = Map.empty[Symbol, Set[(Idempotent, Symbol)]]

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
            octx = optimizable // We can safely extract new calls

            // We transform inner functions at the end in order to collect
            // available idempotent calls before transforming function body
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
            octx = notOptimizable
            cpy.If(tree)(tcond, transform(thenp), transform(elsep))
          }

        case dd @ DefDef(name, tparams, vparamss, tpt, _) =>
          withSavedState {
            // Avoid extracting call in rhs which are not in a block
            // def foo = idem -/-> val $1$ = idem; def foo = $1$
            octx = notOptimizable

            val sym = dd.symbol

            // Filter out idempotent calls not reachable from the function body.
            // For instance, calls whose owner is different from the function owner
            // or not a parent of the function owner
            //val availableSubsts = innerFuns.getOrElse(sym, Set.empty) filter (sym isContainedIn _._2.owner)
            val availableSubsts = Set.empty // FIXME

            substs = availableSubsts.toMap
            blockOwner = sym

            val rhs = transform(dd.rhs)(ctx withOwner sym)
            cpy.DefDef(tree)(name, tparams, vparamss, tpt, rhs)
          }

        case vd @ ValDef(name, _tpt, _) =>
          implicit val ctx: Context = localCtx

          val sym = vd.symbol
          val tpt = transform(_tpt)
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
                ValDef(sym.asTerm, nvd.rhs)
            } getOrElse copy
          }

        case temp: Template =>
          withSavedState {
            // Don't optimize body of a class (could create not wanted fields)
            octx = notOptimizable
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

      val sym = tree.symbol
      var qualifier: Tree = EmptyTree
      var arguments = List.empty[List[Tree]]
      var typeArguments = List.empty[Tree]

      def innerTransform(t: Tree): Tree = {
        assert(t.symbol == sym) // TODO: Remove
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

      val savedOctx = octx
      // Recursively transform and extract calls in qualifier and args.
      // If a subtree is not idempotent, subsequent trees cannot be extracted.
      // Thus the transformation may change the optimisation context.
      val transformed = innerTransform(tree)
      octx = savedOctx

      // To obtain the set of available substitutions of an inner function,
      // we perform the intersection of available substitutions between all
      // the call to the inner function.
      val isInnerFun = (sym is Method) && !sym.owner.isClass
      if (isInnerFun) {
        val substsSet = substs.toSet
        innerFuns += sym -> (innerFuns get sym fold substsSet)(_ intersect substsSet)
      }

      // Is this call an extractable idempotent call
      val idemOption = Idempotent.from(qualifier, sym, typeArguments, arguments)

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
            // Add the extracted value to a buffer to be inserted before the current statement
            treeBuffers.head += valDef
            ref(valDef.symbol)

          // Redundant idempotent call, apply substitution
          case Some(subst) =>
            assert(subst.info.widen =:= tree.tpe.widen) // TODO: Remove
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

      val result = expr

      blockOwner = savedOwner
      substs = savedSubsts
      octx = savedOctx

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
