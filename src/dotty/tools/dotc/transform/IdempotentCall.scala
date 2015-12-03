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

    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      // Function call
      case Apply(_, _) | TypeApply(_, _) | Select(_, _) | Ident(_) =>
        transformMethodCall(tree)

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
        lazy val copy = cpy.ValDef(vd)(name, tpt, rhs)

        // Do not create a new val if duplicate of the current one
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


      case td: TypeDef => td
      case tt: TypTree => tt
      case _           => super.transform(tree)
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

      // Recursively transform inner trees and collect qualifier, arguments and type arguments
      val transformed = withSavedContext { innerTransform(tree) }
      // Is this call an extractable idempotent call
      val idemOption = Idempotent.from(qualifier, tree.symbol, typeArguments, arguments)

      (idemOption fold transformed) { idem =>
        substs get idem match {
          // New idempotent call in optimizable context
          case None if octx.optimizable =>
            val holderName = ctx.freshName().toTermName
            //val valDef = SyntheticValDef(holderName, transformed)
            val holderSym = ctx.newSymbol(ctx.owner, holderName, Synthetic, transformed.tpe.widenExpr, coord = transformed.pos)
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

    /** Evaluate `expr` with the owner `nowner` */
    private def withOwner[T](nowner: Symbol)(expr: => T) = {
      val saved = owner
      owner = nowner
      val result = expr
      owner = saved
      result
    }
  }

  private def isIdempotentExpr(tree: Tree)(implicit ctx: Context): Boolean = tree match {
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

  private def isIdempotentRef(tree: Tree)(implicit ctx: Context): Boolean = {
    if (!tree.tpe.widen.isParameterless) true
    else if (tree.symbol hasAnnotation defn.IdempotentAnnot) true
    else if (!tree.symbol.isStable) false
    else if (tree.symbol is Lazy) true
    else true
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

      val isIdem = isExtractable(fun) && (qual == EmptyTree || isImmutableRef(qual)) &&
        (args forall (_ forall isImmutableRef))

      if (isIdem) {
        val argsTpe = args map (_ map (_.tpe))
        Some(Idempotent(fun, qual.symbol, tpeArgs map (_.tpe), argsTpe))
      }
      else None
    }

    /** @return `true` if `tree` is:
      *          - a constant
      *          - `val`
      *          - `this`
      *          - `super`
      *         `false` otherwise
      */
    private def isImmutableRef(tree: Tree)(implicit ctx : Context): Boolean = tree.tpe match {
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
    private def isExtractable(sym: Symbol)(implicit ctx: Context): Boolean =
      if (sym hasAnnotation defn.IdempotentAnnot) true
      else (sym is Lazy) && !(sym is JavaDefined) // lazy val and singleton objects
  }

  final class OptimizationContext(val optimizable: Boolean) extends AnyVal

  /** Apply substitutions and try to extract idempotent calls */
  final val optimizable = new OptimizationContext(true)

  /** Only apply substitutions */
  final val notOptimizable = new OptimizationContext(false)
}
