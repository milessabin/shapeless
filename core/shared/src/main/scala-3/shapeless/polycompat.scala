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

trait CaseScalaCompat extends CaseInst {
  transparent inline implicit def materializeFromValue1[P, F[_], T]: PolyDefns.Case[P, F[T] :: HNil] =
    ${ PolyMacros.materializeFromValueImpl[P, F[T] :: HNil, T] }

  transparent inline implicit def materializeFromValue2[P, T]: PolyDefns.Case[P, T :: HNil] =
    ${ PolyMacros.materializeFromValueImpl[P, T :: HNil, T] }
}

object PolyMacros {
  def materializeFromValueImpl[P: Type, L <: HList: Type, T: Type](using Quotes): Expr[PolyDefns.Case[P, L]] = {
    import quotes.reflect.*
    TypeRepr.of[P].dealias match {
      case tr: TermRef if tr <:< TypeRepr.of[PolyDefns.~>[?, ?]] =>
        Select.unique(Ref.term(tr), "caseUniv").appliedToType(TypeRepr.of[T]).asExprOf[PolyDefns.Case[P, L]]
      case _ =>
        report.errorAndAbort("Can only materialize cases from singleton values")
    }
  }
}
