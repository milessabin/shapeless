/*
 * Copyright (c) 2011 Miles Sabin 
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

package shapeless.test

import scala.language.experimental.macros

import java.util.regex.Pattern

import scala.reflect.macros.{ Context, TypecheckException }

/**
 * A macro that ensures that a code snippet does not typecheck.
 * 
 * Credit: Stefan Zeiger (@StefanZeiger)
 */
object ShouldNotTypecheck {
  def apply(code: String): Unit = macro applyImplNoExp
  def apply(code: String, expected: String): Unit = macro applyImpl

  def applyImplNoExp(ctx: Context)(code: ctx.Expr[String]) = applyImpl(ctx)(code, null)

  def applyImpl(ctx: Context)(code: ctx.Expr[String], expected: ctx.Expr[String]): ctx.Expr[Unit] = {
    import ctx.universe._

    val Expr(Literal(Constant(codeStr: String))) = code
    val (expPat, expMsg) = expected match {
      case null => (null, "Expected some error.")
      case Expr(Literal(Constant(s: String))) =>
        (Pattern.compile(s, Pattern.CASE_INSENSITIVE), "Expected error matching: "+s)
    }

    try ctx.typeCheck(ctx.parse("{ "+codeStr+" }")) catch { case e: TypecheckException =>
      val msg = e.getMessage
      if((expected ne null) && !(expPat.matcher(msg)).matches)
        ctx.abort(ctx.enclosingPosition, "Type-checking failed in an unexpected way.\n"+
          expMsg+"\nActual error: "+msg)
      else return reify(())
    }

    ctx.abort(ctx.enclosingPosition, "Type-checking succeeded unexpectedly.\n"+expMsg)
  }
}
