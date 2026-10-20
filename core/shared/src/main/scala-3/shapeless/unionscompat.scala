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

trait UnionScalaCompat {
  transparent inline def applyDynamicNamed[U <: Coproduct](inline method: String)(inline elems: (String, Any)*): U =
    ${ UnionMacros.mkUnionNamedImpl[U]('method, 'elems) }

  transparent inline def selectDynamic(inline tpeSelector: String): Any =
    ${ UnionMacros.unionType('tpeSelector) }
}

object UnionMacros {
  def mkUnionNamedImpl[U <: Coproduct: Type](method: Expr[String], elems: Expr[Seq[(String, Any)]])(using Quotes): Expr[U] = {
    import quotes.reflect.*
    val methodString = method.valueOrAbort
    if (methodString != "apply") report.errorAndAbort(s"this method must be called as 'apply' not '$methodString'")
    val elem = elems match {
      case Varargs(Seq(e)) => RecordMacros.promoteElem(e)
      case _ => report.errorAndAbort("only one branch of a union may be inhabited")
    }
    val injTpe = TypeRepr.of[ops.coproduct.Inject].appliedTo(List(TypeRepr.of[U], elem.tpe.widen))
    Implicits.search(injTpe) match {
      case s: ImplicitSearchSuccess => Select.unique(s.tree, "apply").appliedTo(elem).asExprOf[U]
      case f: ImplicitSearchFailure => report.errorAndAbort(f.explanation)
    }
  }

  def unionType(tpeSelector: Expr[String])(using Quotes): Expr[Any] =
    TypeParser.carrier(TypeParser.refinement(List("T" -> RecordMacros.labelledType(tpeSelector.valueOrAbort, "union", MacroUtils.mkCoproductTpe))))
}
