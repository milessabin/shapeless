/*
 * Copyright (c) 2013-16 Miles Sabin
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

import java.util.regex.Pattern

import scala.reflect.macros.{ whitebox, ParseException, TypecheckException }

/**
 * A utility which ensures that a code fragment does not typecheck.
 *
 * Credit: Stefan Zeiger (@StefanZeiger)
 */
object illTyped {
  def apply(code: String): Unit = macro IllTypedMacros.applyImplNoExp
  def apply(code: String, expected: String): Unit = macro IllTypedMacros.applyImpl
}

class IllTypedMacros(val c: whitebox.Context) {
  import c.universe._

  def applyImplNoExp(code: Tree): Tree = applyImpl(code, null)

  def applyImpl(code: Tree, expected: Tree): Tree = {
    val Literal(Constant(codeStr: String)) = (code: @unchecked)
    val (expPat, expMsg) = (expected: @unchecked) match {
      case null => (null, "Expected some error.")
      case Literal(Constant(s: String)) =>
        (Pattern.compile(s, Pattern.CASE_INSENSITIVE | Pattern.DOTALL), "Expected error matching: "+s)
    }

    try {
      val dummy0 = TermName(c.freshName())
      val dummy1 = TermName(c.freshName())
      c.typecheck(c.parse(s"object $dummy0 { val $dummy1 = { $codeStr } }"))
      c.error(c.enclosingPosition, "Type-checking succeeded unexpectedly.\n"+expMsg)
    } catch {
      case e: TypecheckException =>
        val msg = e.getMessage
        if((expected ne null) && !(expPat.matcher(msg)).matches)
          c.error(c.enclosingPosition, "Type-checking failed in an unexpected way.\n"+expMsg+"\nActual error: "+msg)
      case e: ParseException =>
        c.error(c.enclosingPosition, s"Parsing failed.\n${e.getMessage}")
    }

    q"()"
  }
}
