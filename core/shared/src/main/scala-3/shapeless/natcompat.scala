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

trait NatScalaCompat {
  transparent inline implicit def apply(inline i: Int): Nat = ${ NatMacros.materializeWidened('i) }
}

object NatMacros {
  def materializeWidened(i: Expr[Int])(using Quotes): Expr[Nat] =
    mkNatValue(natLiteral(i))

  def natLiteral(i: Expr[Any])(using Quotes): Int = {
    import quotes.reflect.*
    // A literal, or (as Scala 2's constant folding allows) a reference to a constant such as a final val
    val term = i.asTerm.underlying
    (term, term.tpe.widenTermRefByName) match {
      case (Literal(IntConstant(n)), _) if n >= 0 => n
      case (_, ConstantType(IntConstant(n))) if n >= 0 => n
      case _ => report.errorAndAbort(s"Expression ${i.show} does not evaluate to a non-negative Int literal")
    }
  }

  def mkNatTpe(i: Int)(using Quotes): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    val succ = TypeRepr.of[Succ]
    (0 until i).foldLeft(TypeRepr.of[_0])((acc, _) => succ.appliedTo(acc))
  }

  def mkNatValue(i: Int)(using Quotes): Expr[Nat] = {
    import quotes.reflect.*
    if (i == 0) '{ Nat._0 }
    else {
      val tpe = mkNatTpe(i)
      Select(New(Inferred(tpe)), tpe.typeSymbol.primaryConstructor).appliedToTypes(tpe.typeArgs).appliedToNone.asExprOf[Nat]
    }
  }
}
