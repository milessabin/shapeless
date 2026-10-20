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

import scala.quoted.*

trait TestScalaCompat {
  /**
   * Checks that `t2` conforms to the type of `t1`. On Scala 3 this is a macro, so that, as on Scala 2, the type of `t1`
   * isn't widened (except for literals) before `t2` is checked against it.
   */
  transparent inline def sameTyped[T](inline t1: T)(inline t2: Any): Unit = ${ TestMacros.sameTypedImpl('t1, 't2) }

  inline def showType[T]: String = ${ TestMacros.showTypeNoValue[T] }

  inline def showType[T](t: => T): String = ${ TestMacros.showTypeNoValue[T] }
}

object TestMacros {
  def sameTypedImpl[T](t1: Expr[T], t2: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    def tpeOf(t: Term): TypeRepr = t.underlyingArgument match {
      case lit @ Literal(_) => lit.tpe.widen
      case other => MacroUtils.normalize(other.tpe.widenTermRefByName)
    }
    val expected = tpeOf(t1.asTerm)
    val found = MacroUtils.normalize(t2.asTerm.underlyingArgument.tpe.widenTermRefByName)
    if (!(found <:< expected))
      report.error(s"type mismatch;\n found   : ${found.show}\n required: ${expected.show}", t2)
    '{ () }
  }

  def showTypeNoValue[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[T].dealias.show)
  }
}
