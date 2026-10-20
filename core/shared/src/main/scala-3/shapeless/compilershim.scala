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

package shapeless

import scala.quoted.*
import scala.util.control.Exception.catching

/**
 * The parts of the Scala 3 implementation which depend on compiler internals.
 *
 * Compiler internals aren't binary compatible between Scala 3 versions, so the implementations of this trait are
 * compiled separately against each range of compiler versions with compatible internals, and the one for the running
 * compiler is selected by [[CompilerShims]]. This file is compiled with each of them as well as with shapeless, so it
 * must only depend on the Scala 3 library.
 */
trait CompilerShim {
  /** The version of the running compiler. */
  def compilerVersion: String

  /**
   * Whether the enclosing implicit search, if it's for the instance of `B[T]` named `instanceName`, is for a type with
   * type variables yet to be instantiated.
   */
  def isUndetermined[B](barrier: Type[B], instanceName: String)(using Quotes): Boolean

  /**
   * Searches for an implicit value of type `T` by-name, as a barrier between the enclosing implicit search and the
   * nested one, used to resolve implicits of type `B[T]` where `B` is eg. `Lazy`.
   *
   * Searches separated by barriers are checked for divergence in the same way as Scala 2 checks implicits for
   * divergence, rather than as Scala 3 does, while recursive searches still tie the knot as by-name implicits do.
   *
   * @param barrier the barrier type, `B[Any]`
   * @param requested the type to search for if the enclosing search isn't for an instance named `instanceName`
   * @param instanceName the name of the implicit definition of `B[T]` which calls this
   * @param instantiate whether to instantiate the type variables of the enclosing search's type from the type of the
   *                    implicit value found, rather than leaving that to the caller
   * @return the implicit value found, or the reason none was found
   */
  def searchByName[B, T](barrier: Type[B], requested: Type[T], instanceName: String, instantiate: Boolean)(
    using Quotes
  ): Either[String, Expr[Any]]

  /**
   * Searches for an implicit value of type `T` as `Implicits.search` does, to initialize the val (or lazy val) `owner`,
   * as Scala 2's `cachedImplicit` does: neither `owner` nor the other vals defined alongside it are candidates.
   */
  def searchForVal[T](using q: Quotes)(requested: Type[T], owner: q.reflect.Symbol): q.reflect.ImplicitSearchResult

  /**
   * Typechecks `code`, a type if `isType` or otherwise an expression, as if it were written at the call site of the
   * macro expansion, so that names are resolved in its scope, including its imports and local definitions. As on
   * Scala 2, the type of an expression ascribed a type (`e: T`) is that of `e`, typechecked with `T` expected.
   *
   * @return the type of `code`, or the errors reported typechecking it
   */
  def typeCheck(using q: Quotes)(code: String, isType: Boolean): Either[String, q.reflect.TypeRepr]
}

/** Selects the [[CompilerShim]] for the running compiler. */
object CompilerShims {
  /**
   * The classes of the shims included with shapeless, each with the earliest compiler version it supports (major,
   * minor), newest first. These must agree with `compilerShims` in build.sbt.
   */
  val variants: List[(String, (Int, Int))] = List(
    "shapeless.compilershims.v3_7.Shim" -> (3, 7),
    "shapeless.compilershims.v3_3.Shim" -> (3, 3)
  )

  lazy val loaded: List[(CompilerShim, (Int, Int))] =
    variants.flatMap { case (name, since) => load(name).map(_ -> since) }

  lazy val runningCompilerVersion: Option[String] =
    loaded.headOption.flatMap { case (shim, _) => catching(classOf[LinkageError]).opt(shim.compilerVersion) }

  lazy val forRunningCompiler: Option[CompilerShim] =
    runningCompilerVersion.flatMap(majorMinor).flatMap { version =>
      loaded.collectFirst { case (shim, since) if Ordering[(Int, Int)].gteq(version, since) => shim }
    }

  /**
   * Runs `run` with the shim for the running compiler, for features which fall back to a more limited implementation
   * where there is none, or where it is incompatible with the running compiler (yielding `None`).
   */
  def optionally[T](run: CompilerShim => T): Option[T] =
    forRunningCompiler.flatMap(shim => catching(classOf[LinkageError]).opt(run(shim)))

  /** Runs a search with a shim, reporting a shim incompatible with the running compiler as a failed search. */
  def search[T](run: => Either[String, T]): Either[String, T] =
    catching(classOf[LinkageError]).either(run).left.map { e =>
      s"shapeless doesn't support Scala ${runningCompilerVersion.getOrElse("?")}: $e"
    }.flatten

  def load(name: String): Option[CompilerShim] =
    catching(classOf[ReflectiveOperationException], classOf[LinkageError])
      .opt(Class.forName(name, true, getClass.getClassLoader).getDeclaredConstructor().newInstance())
      .collect { case shim: CompilerShim => shim }

  def majorMinor(version: String): Option[(Int, Int)] =
    "^(\\d+)\\.(\\d+)".r.findFirstMatchIn(version).map(m => (m.group(1).toInt, m.group(2).toInt))
}
