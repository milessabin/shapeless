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

import shapeless.PolyDefns.Case

import language.experimental.macros

import reflect.macros.whitebox

trait CaseScalaCompat {

  implicit def materializeFromValue1[P, F[_], T]: Case[P, F[T] :: HNil] =
    macro PolyMacros.materializeFromValueImpl[P, F[T], T]

  implicit def materializeFromValue2[P, T]: Case[P, T :: HNil] =
    macro PolyMacros.materializeFromValueImpl[P, T, T]
}

class PolyMacros(val c: whitebox.Context) {
  import c.universe._

  import PolyDefns.Case

  def materializeFromValueImpl[P: WeakTypeTag, FT: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val pTpe = weakTypeOf[P]
    val ftTpe = weakTypeOf[FT]
    val tTpe = weakTypeOf[T]

    val recTpe = weakTypeOf[Case[P, FT :: HNil]]
    if(c.openImplicits.tail.exists(_.pt =:= recTpe))
      c.abort(c.enclosingPosition, s"Diverging implicit expansion for Case.Aux[$pTpe, $ftTpe :: HNil]")

    val value = pTpe match {
      case SingleType(_, f) => f
      case other            => c.abort(c.enclosingPosition, "Can only materialize cases from singleton values")
    }

    q""" $value.caseUniv[$tTpe] """
  }
}
