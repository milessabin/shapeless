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

/**
 * The value of a [[Lazy]].
 *
 * Scala 3 doesn't allow a `lazy val` to implement an abstract `val`, as Scala 2 does, so on Scala 3 subclasses of
 * `Lazy` implement `computeValue` rather than `value`.
 */
trait LazyValue[+T] {
  protected def computeValue: T

  lazy val value: T = computeValue
}

object LazyValue {
  def mkLazy[T](t: => T): Lazy[T] =
    new Lazy[T] {
      protected def computeValue: T = t
    }
}

/*
 * On Scala 3 a `Lazy` implicit is a barrier between the enclosing implicit search and the nested one (see
 * `CompilerShim.searchByName`), so that recursive and nested derivations which Scala 2 accepts aren't deemed divergent.
 * This depends on compiler internals; with a compiler for which shapeless has no `CompilerShim`, `Lazy` falls back to
 * an ordinary by-name implicit.
 */
trait LazyInstances extends LazyUndeterminedInstances {
  transparent inline implicit def mkLazy[I]: Lazy[I] = ${ LazyMacros.mkLazyImpl[I] }
}

trait LazyUndeterminedInstances extends LazyByNameInstances {
  /**
   * For a `Lazy[I]` where `I` has undetermined type variables (eg. `Lazy[Generic.Aux[T, R]]`), from Scala 3.10.
   *
   * Before expanding `mkLazy` the compiler approximates the undetermined type variables in its type argument `I` (eg. as
   * `Generic.Aux[T, Any]`), and from Scala 3.10 it requires the expansion to conform to `Lazy[I]` with that
   * approximation, which the instance found (eg. `Lazy[Generic.Aux[T, Int :: HNil]]`) doesn't. So instead this is a
   * candidate for any `Lazy[I]`, and the macro instantiates the type variables itself.
   */
  transparent inline implicit def mkLazyUndetermined: Lazy[Nothing] =
    LazyMacros.lazyUndetermined.asInstanceOf[Lazy[Nothing]]
}

trait LazyByNameInstances {
  implicit def mkLazyByName[I](implicit noShim: LazyMacros.NoCompilerShim, i: => I): Lazy[I] = Lazy(i)
}

object LazyMacros {
  def mkLazyImpl[I: Type](using Quotes): Expr[Lazy[I]] = {
    import quotes.reflect.*

    val found = withShim { shim =>
      if (checksExpansionTypes && shim.isUndetermined(Type.of[Lazy[Any]], "mkLazy"))
        Left("undetermined type variables are instantiated by mkLazyUndetermined")
      else shim.searchByName(Type.of[Lazy[Any]], Type.of[I], "mkLazy", instantiate = false)
    }

    // The type of the instance found may be more precise than `I`, whose type variables have been instantiated already,
    // and the result is typed accordingly so that the expansion instantiates them. Casting the expression is safe as
    // its type is `Lazy[u]` with `u <: I`
    found.asTerm.tpe.widen.asType match {
      case '[u] => '{ LazyValue.mkLazy[u](${ found.asExprOf[u] }) }.asInstanceOf[Expr[Lazy[I]]]
    }
  }

  transparent inline def lazyUndetermined: Lazy[Any] = ${ lazyUndeterminedImpl }

  def lazyUndeterminedImpl(using Quotes): Expr[Lazy[Any]] = {
    import quotes.reflect.*

    val found = withShim { shim =>
      if (checksExpansionTypes && shim.isUndetermined(Type.of[Lazy[Any]], "mkLazyUndetermined"))
        shim.searchByName(Type.of[Lazy[Any]], Type.of[Nothing], "mkLazyUndetermined", instantiate = true)
      else Left("only undetermined type variables are instantiated by mkLazyUndetermined")
    }

    found.asTerm.tpe.widen.asType match {
      case '[u] => '{ LazyValue.mkLazy[u](${ found.asExprOf[u] }) }
    }
  }

  def withShim(search: CompilerShim => Either[String, Expr[Any]])(using Quotes): Expr[Any] =
    CompilerShims.forRunningCompiler
      .toRight(s"no compiler support for Lazy with Scala ${CompilerShims.runningCompilerVersion.getOrElse("?")}")
      .flatMap(shim => CompilerShims.search(search(shim)))
      .fold(quotes.reflect.report.errorAndAbort(_), identity)

  /** Whether the running compiler requires macro expansions to conform to the expected type, as Scala 3.10 does. */
  def checksExpansionTypes: Boolean =
    CompilerShims.runningCompilerVersion.flatMap(CompilerShims.majorMinor).exists(Ordering[(Int, Int)].gteq(_, (3, 10)))

  /** Evidence that there is no `CompilerShim` for the running compiler. */
  final class NoCompilerShim

  object NoCompilerShim {
    transparent inline implicit def instance: NoCompilerShim = ${ instanceImpl }

    def instanceImpl(using Quotes): Expr[NoCompilerShim] =
      if (CompilerShims.forRunningCompiler.isEmpty) '{ new NoCompilerShim }
      else quotes.reflect.report.errorAndAbort("there is a compiler shim")
  }
}
