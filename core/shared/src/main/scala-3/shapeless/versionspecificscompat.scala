/*
 * Copyright (c) 2011-16 Miles Sabin
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

package shapeless

import scala.quoted.*
import scala.util.NotGiven

trait ScalaVersionSpecifics {
  private[shapeless] type IterableTypeableResult[CC[X] <: Iterable[X], T] = CC[T]

  /**
   * Resolves an implicit value of type `T` to initialize a `val` or `lazy val`, excluding that val and the other vals
   * defined alongside it from the implicit search.
   *
   * Excluding them depends on compiler internals. With a compiler for which shapeless has no `CompilerShim`, they
   * aren't excluded, and where the search would select one of them this reports an error rather than yielding a
   * self-referential value.
   */
  transparent inline def cachedImplicit[T]: T = ${ CachedImplicitMacros.cachedImplicitImpl[T] }
}

object CachedImplicitMacros {
  def cachedImplicitImpl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*

    // Note: the flags of the definition being initialized mustn't be inspected, as that would force its type
    def isParam(s: Symbol): Boolean = s.owner.isDefDef && s.owner.paramSymss.flatten.contains(s)

    def enclosingVal(s: Symbol): Symbol =
      if (s.isNoSymbol || s.isDefDef || s.isClassDef) Symbol.noSymbol
      else if (s.isValDef && !isParam(s)) s
      else enclosingVal(s.owner)

    val start = if (Symbol.spliceOwner.name == "macro") Symbol.spliceOwner.owner else Symbol.spliceOwner
    val owner = enclosingVal(start)
    if (owner.isNoSymbol)
      report.errorAndAbort("cachedImplicit should only be used to initialize vals and lazy vals")

    CompilerShims.optionally(_.searchForVal(Type.of[T], owner)).getOrElse(searchChecked[T](owner)) match {
      case s: ImplicitSearchSuccess => s.tree.asExprOf[T]
      case _: ImplicitSearchFailure => report.errorAndAbort(implicitNotFoundMessage(TypeRepr.of[T]))
    }
  }

  /** As on Scala 2, the `@implicitNotFound` message of the type searched for, or a generic one. */
  def implicitNotFoundMessage(using Quotes)(tpe: quotes.reflect.TypeRepr): String = {
    import quotes.reflect.*
    val sym = tpe.typeSymbol
    val annotation = sym.getAnnotation(TypeRepr.of[scala.annotation.implicitNotFound].typeSymbol).collect {
      case Apply(_, List(Literal(StringConstant(msg)))) => msg
    }
    annotation.fold(s"Implicit value of type ${tpe.show(using Printer.TypeReprShortCode)} not found") { msg =>
      val params = sym.declaredTypes.filter(_.isTypeParam).map(_.name)
      val args = tpe.baseType(sym).typeArgs.map(_.show(using Printer.TypeReprShortCode))
      params.zip(args).foldLeft(msg) { case (m, (param, arg)) => m.replace(s"$${$param}", arg) }
    }
  }

  /** Searches for an implicit value of type `T`, rejecting `owner` or an implicit val defined after it. */
  def searchChecked[T: Type](using Quotes)(owner: quotes.reflect.Symbol): quotes.reflect.ImplicitSearchResult = {
    import quotes.reflect.*

    def refs(t: Tree): List[Symbol] =
      new TreeAccumulator[List[Symbol]] {
        def foldTree(acc: List[Symbol], tree: Tree)(o: Symbol): List[Symbol] = tree match {
          case Ident(_) | Select(_, _) => foldOverTree(tree.symbol :: acc, tree)(o)
          case _ => foldOverTree(acc, tree)(o)
        }
      }.foldTree(Nil, t)(Symbol.spliceOwner)

    def isLaterSibling(sym: Symbol): Boolean =
      sym != owner && sym.owner == owner.owner && sym.isValDef && sym.flags.is(Flags.Implicit) &&
        sym.pos.zip(owner.pos).forall { case (s, o) => s.start >= o.start }

    Implicits.search(TypeRepr.of[T]) match {
      case s: ImplicitSearchSuccess if refs(s.tree).exists(sym => sym == owner || isLaterSibling(sym)) =>
        report.errorAndAbort(
          s"cachedImplicit: the implicit value of type ${Type.show[T]} resolves to the definition being initialized " +
            s"(${owner.name}) or to an implicit val defined after it. On Scala 3 provide the instance explicitly instead."
        )
      case result => result
    }
  }
}

trait NeqInstances {
  implicit def neq[A, B](implicit ev: NotGiven[A =:= B]): A =:!= B = new =:!=[A, B] {}
}

trait NsubInstances {
  implicit def nsub[A, B](implicit ev: NotGiven[A <:< B]): A <:!< B = new <:!<[A, B] {}
}
