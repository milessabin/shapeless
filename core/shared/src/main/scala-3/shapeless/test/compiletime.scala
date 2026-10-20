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
package test

import java.util.concurrent.atomic.AtomicLong

import scala.compiletime.testing.typeCheckErrors
import scala.concurrent.duration.FiniteDuration
import scala.quoted.*

/**
 * Utility that measures the compilation time of a code fragment.
 *
 * `compileTime` takes a code fragment as `String`, measures the time
 * it takes to parse and typecheck it and returns that time as a
 * `FiniteDuration`.
 *
 * On Scala 3 the time is measured between macro expansions preceding and following the typechecking of the fragment,
 * so it is an approximation.
 *
 * Example: {{{
 * scala> compileTime(""" Generic[(Int, Option[String])] """)
 * res0: FiniteDuration = 43153718 nanoseconds
 * }}}
 */
object compileTime {
  transparent inline def apply(inline code: String): FiniteDuration = {
    CompileTimeMacros.mark()
    CompileTimeMacros.check(typeCheckErrors(code))
    CompileTimeMacros.elapsed()
  }
}

object CompileTimeMacros {
  val started: AtomicLong = new AtomicLong(0L)

  transparent inline def mark(): Unit = ${ markImpl }
  transparent inline def check(inline errors: List[scala.compiletime.testing.Error]): Unit = ${ checkImpl('errors) }
  transparent inline def elapsed(): FiniteDuration = ${ elapsedImpl }

  def markImpl(using Quotes): Expr[Unit] = {
    started.set(System.nanoTime())
    '{ () }
  }

  def checkImpl(errors: Expr[List[scala.compiletime.testing.Error]])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    IllTypedMacros.typeCheckErrorMessages(errors).headOption.foreach { case (msg, _) => report.errorAndAbort(msg) }
    '{ () }
  }

  def elapsedImpl(using Quotes): Expr[FiniteDuration] = {
    val elapsed = System.nanoTime() - started.get
    '{ scala.concurrent.duration.Duration.fromNanos(${ Expr(elapsed) }) }
  }
}
