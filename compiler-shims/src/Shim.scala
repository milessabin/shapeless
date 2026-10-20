/*
 * Copyright (c) 2026 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// The build compiles this source against each supported range of compiler versions, putting each in its own
// subpackage of this package (see `compilerShim` in build.sbt)
package shapeless.compilershims

import scala.quoted.{Expr, Quotes, Type}
import scala.quoted.runtime.impl.QuotesImpl
import scala.util.control.Exception.catching

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.Trees.{PackageDef, TypeDef, Typed, ValDef}
import dotty.tools.dotc.Run
import dotty.tools.dotc.config.Properties
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Flags.{GivenOrImplicitVal, Invisible, Macro, Method, Module}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.TypeComparer
import dotty.tools.dotc.core.Symbols.{defn, toDenot, Symbol}
import dotty.tools.dotc.core.Types.{AndType, AppliedType, ExprType, HKTypeLambda, OrType, RefinedType, TypeParamRef, TypeVar}
import dotty.tools.dotc.core.Types.{Type => TypeRepr}
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.typer.{Inferencing, OpenSearch, SearchHistory}
import dotty.tools.dotc.typer.Implicits.{Candidate, ContextualImplicits, SearchFailure, SearchSuccess}
import dotty.tools.dotc.util.{EqHashMap, SourceFile}

import shapeless.CompilerShim

class Shim extends CompilerShim {
  def compilerVersion: String = Properties.versionNumberString

  def isUndetermined[B](barrier: Type[B], instanceName: String)(using q: Quotes): Boolean =
    q match {
      case qi: QuotesImpl => Barrier(qi, barrier, Type.of[Any](using qi), instanceName).isUndetermined
    }

  def searchByName[B, T](barrier: Type[B], requested: Type[T], instanceName: String, instantiate: Boolean)(
    using q: Quotes
  ): Either[String, Expr[Any]] =
    q match {
      case qi: QuotesImpl => Barrier(qi, barrier, requested, instanceName).search(instantiate)
    }

  // `q` is a `QuotesImpl`, whose reflected trees, types and symbols are those of the compiler
  def searchForVal[T](using q: Quotes)(requested: Type[T], owner: q.reflect.Symbol): q.reflect.ImplicitSearchResult =
    q match {
      case qi: QuotesImpl =>
        given Context = qi.ctx
        val pt = qi.reflect.TypeRepr.of[T](using requested)
        val span = qi.reflect.Position.ofMacroExpansion.span
        val ignored = valsAlongside(owner.asInstanceOf[Symbol])
        val found = whileHidden(ignored.filter(_.isOneOf(GivenOrImplicitVal))) {
          ImplicitSearches.inferImplicitArgIgnoring(pt, span, ignored)
        }
        // As `Implicits.search`, so that the type of the value found has no uninstantiated type variables
        Inferencing.fullyDefinedType(found.tpe, "", found)
        found.asInstanceOf[q.reflect.ImplicitSearchResult]
    }

  /**
   * The vals and lazy vals defined alongside `owner` (in the same class or block), including `owner`. Only flags which
   * are known before definitions are completed are inspected, so that no definitions' types are forced.
   */
  def valsAlongside(owner: Symbol)(using Context): Set[Symbol] = {
    val scopeOwner = owner.owner
    val defined =
      if (scopeOwner.isClass) scopeOwner.unforcedDecls.toList
      else ctx.outersIterator.filter(_.owner == scopeOwner).flatMap(_.scope.toList).toList
    defined.filter(sym => sym.owner == scopeOwner && sym.isTerm && !sym.isOneOf(Method | Module)).toSet + owner
  }

  /**
   * Runs `search` with the implicits `syms` hidden, so that their types aren't forced. Implicit search forces the type
   * of every candidate before filtering any out, and the type of an implicit defined alongside the val being
   * initialized may depend on that val, as in `implicit val gen: Generic.Aux[Quux, gen0.Repr]`, which would then be a
   * cyclic reference. During typer a symbol flagged `Invisible` isn't a member of its owner, so isn't among the
   * implicits of an implicit scope, and is absent, so is dropped from the implicits of the context before its type is
   * forced.
   *
   * The implicits which the context makes visible are computed beforehand, so that they include `syms`. The memoized
   * eligible candidates are discarded beforehand, as those of an implicit scope include `syms`, and afterwards, as those
   * computed meanwhile don't. If those memos can't be reached nothing is hidden.
   */
  def whileHidden[T](syms: Set[Symbol])(search: => T)(using Context): T = {
    val hidden = syms.filterNot(_.is(Invisible))
    eligibleMemos.filter(_ => hidden.nonEmpty).fold(search) { memos =>
      memos.foreach(_.clear(true))
      hidden.foreach(_.setFlag(Invisible))
      try search
      finally {
        hidden.foreach(_.resetFlag(Invisible))
        memos.foreach(_.clear(true))
      }
    }
  }

  /**
   * The memoized eligible candidates of implicit search: those of each implicit scope for this run, and those of each
   * level of the implicits which the context makes visible. Neither is accessible from Scala, but both have been
   * stable from 3.3.0 to at least 3.10.
   */
  def eligibleMemos(using Context): Option[List[EqHashMap[?, ?]]] = {
    val contextual = List.unfold(ctx.implicits)(Option(_).map(implicits => (implicits, implicits.outerImplicits)))
    catching(classOf[ReflectiveOperationException], classOf[RuntimeException]).opt {
      val scopes = classOf[Run].getMethod("dotty$tools$dotc$typer$ImplicitRunInfo$$implicitScopeCache").invoke(ctx.run)
      val eligibleCache = classOf[ContextualImplicits].getDeclaredField("eligibleCache")
      eligibleCache.setAccessible(true)
      (scopes :: contextual.map(eligibleCache.get)).collect { case memo: EqHashMap[?, ?] => memo }
    }.filter(_.length == contextual.length + 1)
  }

  def typeCheck(using q: Quotes)(code: String, isType: Boolean): Either[String, q.reflect.TypeRepr] =
    q match {
      case qi: QuotesImpl => CallSiteTyper(qi).typeCheck(code, isType).map(_.asInstanceOf[q.reflect.TypeRepr])
    }
}

/*
 * Typechecks code at the call site of a macro expansion, as Scala 2's `c.typecheck(c.parse(code))` does.
 *
 * The code is parsed as the right hand side of a definition in a compilation unit of its own, and typechecked in the
 * context of the macro expansion, which is that of its call site. Errors are reported as the result rather than to the
 * compiler, and the constraints typechecking adds are only kept if there are no errors.
 */
final case class CallSiteTyper(qi: QuotesImpl) {
  given Context = qi.ctx

  def typeCheck(code: String, isType: Boolean): Either[String, TypeRepr] = {
    val reporter = StoreReporter(ctx.reporter)
    val source = SourceFile.virtual("<shapeless>", if (isType) s"type T = $code" else s"val v = $code")
    val typingCtx = ctx.fresh.setNewTyperState().setReporter(reporter).setSource(source)
    val parsed = Parsing.parseUnit(source)(using typingCtx)
    def errors: Option[String] =
      Option(reporter.removeBufferedMessages(using typingCtx).map(_.message)).filter(_.nonEmpty).map(_.mkString("; "))

    val tpe = errors.toLeft(()).flatMap { _ =>
      definitionRhs(parsed, isType).toRight(s"malformed ${if (isType) "type" else "expression"}: $code")
    }.map { rhs =>
      val typer = ctx.typer
      (rhs, isType) match {
        case (_, true) => typer.typedType(rhs)(using typingCtx).tpe
        case (Typed(expr, tpt), false) =>
          typer.typedExpr(expr, typer.typedType(tpt)(using typingCtx).tpe)(using typingCtx).tpe
        case _ => typer.typedExpr(rhs)(using typingCtx).tpe
      }
    }
    tpe.flatMap { t =>
      errors.toLeft {
        typingCtx.typerState.commit()
        t
      }
    }
  }

  /** The right hand side of the only definition in `tree`, a compilation unit, if that's a type alias or val. */
  def definitionRhs(tree: untpd.Tree, isType: Boolean): Option[untpd.Tree] =
    tree match {
      case PackageDef(_, List(TypeDef(_, rhs))) if isType => Some(rhs)
      case PackageDef(_, List(vd @ ValDef(_, _, _))) if !isType => Some(vd.rhs)
      case _ => None
    }
}

/*
 * A by-name implicit search behind a barrier.
 *
 * Scala 3 checks an implicit search for divergence by comparing it with every enclosing search for the same implicit
 * candidate, by-name or not. It measures the size of types including type lambda bodies and the members of
 * refinements (eg. the `Out` of an `Aux` type), and it measures the enclosing searches when they started, when their
 * type variables were yet to be instantiated. So searches which Scala 2 accepts, eg. `Generic1` based derivations for
 * nested type constructors, or `everywhere` on nested data, are deemed divergent, and fail or resolve to a lower
 * priority instance instead.
 *
 * The search is done with every enclosing search attributed to a dummy candidate, which no nested search matches, so
 * that Scala 3 only compares searches between barriers. Searches across barriers are compared here instead, in the
 * way Scala 2 compares them. The enclosing searches are kept, so that a recursive search still ties the knot with an
 * enclosing by-name search.
 */
final case class Barrier[B, T](qi: QuotesImpl, barrier: Type[B], requested: Type[T], instanceName: String) {
  given Context = qi.ctx

  val barrierClass: Symbol = qi.reflect.TypeRepr.of[B](using barrier).typeSymbol

  // The type of the enclosing search, if it's for the instance named `instanceName`, and the searches enclosing it
  val (own, enclosing) = ctx.searchHistory.openSearchPairs match {
    case (cand, pt) :: rest if cand.ref.symbol.name.toString == instanceName => (barrierArg(pt), rest)
    case pairs => (None, pairs)
  }

  def isUndetermined: Boolean =
    own.exists(_.existsPart {
      case tvar: TypeVar => !tvar.isInstantiated
      case _ => false
    })

  def search(instantiate: Boolean): Either[String, Expr[Any]] = {
    ctx.searchHistory.openSearchPairs.foreach { case (_, pt) => instantiateHigherKinded(pt) }
    // The enclosing search's type has its type variables uninstantiated, unlike `requested`
    val target = own.getOrElse(qi.reflect.TypeRepr.of[T](using requested))
    if (enclosing.flatMap { case (_, pt) => barrierArg(pt) }.exists(dominates(target, _)))
      Left(s"diverging implicit expansion for type ${target.show}")
    else {
      val dummy = Candidate(defn.Predef_undefined.termRef, Candidate.Value, 0)
      val history = ctx.searchHistory.openSearchPairs.foldRight(ctx.searchHistory.root: SearchHistory) {
        case ((_, pt), outer) => OpenSearch(dummy, pt, outer)
      }
      val searchCtx = ctx.fresh.setSearchHistory(history).setOwner(outsideMacros)
      val span = qi.reflect.Position.ofMacroExpansion.span
      ctx.typer.inferImplicit(ExprType(target), tpd.EmptyTree, span)(using searchCtx) match {
        // Instantiating the type variables of `target` from the instance found constrains them in the enclosing search
        case found: SearchSuccess if instantiate && !(found.tree.tpe.widen <:< target) =>
          Left(s"${found.tree.tpe.widen.show} doesn't conform to ${target.show}")
        case found: SearchSuccess => Right(qi.reflect.TreeMethods.asExpr(found.tree))
        case failed: SearchFailure => Left(failed.reason.explanation)
      }
    }
  }

  /**
   * Instantiates the type constructor variables of `tpe` which are bounded only from above, by a type other than `Any`,
   * to that bound. Such a variable is determined only by an expected type in which it appears covariantly (eg. the `FT`
   * of `IsHCons1.apply` for an expected `IsHCons1[L, FH, [t[_]] =>> TC[t, G]]`), as Scala 2's macros take it. Left
   * uninstantiated, it would be searched for as an application of a type variable (eg. `Lazy[FT[T]]`), whose implicit
   * scope doesn't include the companion of the type class of its bound (eg. that of `TC`), and nested searches compared
   * with the enclosing ones may add bounds to it (eg. `FT <: Functor` from a nested `IsHCons1[L, Functor, Functor]`). So
   * this is done for the types of all the open searches, the earliest point at which a barrier sees them.
   */
  def instantiateHigherKinded(tpe: TypeRepr): Unit =
    tpe.foreachPart {
      case tvar: TypeVar if !tvar.isInstantiated && tvar.origin.typeParams.nonEmpty && !tvar.hasLowerBound =>
        val hi = TypeComparer.fullUpperBound(tvar.origin)
        val hiResult = hi match {
          case lambda: HKTypeLambda => lambda.resType
          case t => t
        }
        if (!hiResult.isRef(defn.AnyClass)) tvar.instantiate(fromBelow = false)
      case _ =>
    }

  def barrierArg(tpe: TypeRepr): Option[TypeRepr] =
    Some(tpe.widenExpr.baseType(barrierClass)).filter(_.exists).flatMap(_.argInfos.headOption)

  /** As Scala 2's `dominates`: whether a search for `dtor` within one for `dted` might diverge. */
  def dominates(dtor: TypeRepr, dted: TypeRepr): Boolean =
    dtor.typeSymbol == dted.typeSymbol && symbols(dtor) == symbols(dted) && {
      val c = complexity(dtor).compare(complexity(dted))
      if (c == 0) dtor =:= dted else c > 0
    }

  /** As Scala 2's `complexity`, which disregards the members of refinements and the bodies of type lambdas. */
  def complexity(tpe: TypeRepr): Int = tpe.dealias match {
    case AppliedType(_, args) => 1 + args.map(complexity).sum
    case RefinedType(parent, _, _) => 1 + complexity(parent)
    case AndType(l, r) => 1 + complexity(l) + complexity(r)
    case OrType(l, r) => 1 + complexity(l) + complexity(r)
    case _ => 1
  }

  def symbols(tpe: TypeRepr): Set[Symbol] = tpe.dealias match {
    case AppliedType(tycon, args) => args.foldLeft(Set(tycon.typeSymbol))(_ ++ symbols(_))
    case RefinedType(parent, _, _) => symbols(parent)
    case AndType(l, r) => symbols(l) ++ symbols(r)
    case OrType(l, r) => symbols(l) ++ symbols(r)
    case _: TypeVar | _: TypeParamRef | _: HKTypeLambda => Set.empty
    case t => Set(t.typeSymbol)
  }

  /**
   * The owner of the outermost macro expansion enclosing this one. The implicit values found are referenced from
   * there, the scope of the dictionary of any by-name implicits the search ties the knot with.
   */
  def outsideMacros: Symbol = {
    val owners = ctx.owner.ownersIterator.toList
    owners.lift(owners.lastIndexWhere(isMacroOwner) + 1).getOrElse(ctx.owner)
  }

  /** As `Splicer.isMacroOwner`, which Scala 3.3.0 lacks: whether `sym` owns a macro expansion. */
  def isMacroOwner(sym: Symbol): Boolean =
    sym.is(Macro, butNot = Method) && sym.name == nme.MACROkw
}
