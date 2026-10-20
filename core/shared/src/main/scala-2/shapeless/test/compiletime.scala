/*
 * Copyright (c) 2016 Frank S. Thomas
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

import scala.language.experimental.macros

import scala.concurrent.duration.FiniteDuration
import scala.reflect.macros.blackbox

/**
 * Utility that measures the compilation time of a code fragment.
 *
 * `compileTime` takes a code fragment as `String`, measures the time
 * it takes to parse and typecheck it and returns that time as a
 * `FiniteDuration`.
 *
 * Example: {{{
 * scala> compileTime(""" Generic[(Int, Option[String])] """)
 * res0: FiniteDuration = 43153718 nanoseconds
 * }}}
 */
object compileTime {
  def apply(code: String): FiniteDuration = macro CompileTimeMacros.applyImpl
}

class CompileTimeMacros(val c: blackbox.Context) {
  import c.universe._

  def applyImpl(code: Tree): Tree = {
    def wallClock(codeStr: String): Long = {
      try {
        val t1 = System.nanoTime()
        c.typecheck(c.parse(codeStr))
        val t2 = System.nanoTime()
        t2 - t1
      } catch {
        case ex: Exception => c.abort(c.enclosingPosition, ex.getMessage)
      }
    }

    val Literal(Constant(codeStr: String)) = (code: @unchecked)
    val elapsedTime = wallClock(codeStr)

    q"_root_.scala.concurrent.duration.Duration.fromNanos($elapsedTime)"
  }
}
