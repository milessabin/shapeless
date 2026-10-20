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

// See `Materialized` for why there are two macros

trait LabellingScalaCompat extends LabellingScalaCompat0 {
  transparent inline implicit def mkLabelling[T, L <: HList]: Labelling.Aux[T, L] = ${ LabelledMacros.materialize[T, L] }

  transparent inline implicit def materializeLabelling[T]: Materialized[Labelling[T]] =
    ${ LabelledMacros.materializedLabelling[T] }
}

trait LabellingScalaCompat0 {
  implicit def materializeAux[T, L <: HList](
    implicit labelling: Materialized[Labelling[T]], ev: labelling.value.Out =:= L
  ): Labelling.Aux[T, L] =
    ev.substituteCo[[l] =>> Labelling[T] { type Out = l }](labelling.value)
}

object LabelledMacros {
  def materialize[T: Type, L <: HList: Type](using Quotes): Expr[Labelling.Aux[T, L]] =
    mkLabelling[T].asInstanceOf[Expr[Labelling.Aux[T, L]]]

  def materializedLabelling[T: Type](using Quotes): Expr[Materialized[Labelling[T]]] =
    MacroUtils.materialized(mkLabelling[T])

  def mkLabelling[T: Type](using Quotes): Expr[Labelling[T]] = {
    import quotes.reflect.*
    val labels = GenericMacros.labelsOf(TypeRepr.of[T].dealias).map(l => Literal(StringConstant(l)))
    val hlist = MacroUtils.mkHList(labels)
    Select.unique(Ref(TypeRepr.of[Labelling.type].termSymbol), "instance")
      .appliedToTypes(List(TypeRepr.of[T], hlist.tpe.widen))
      .appliedTo(hlist)
      .asExprOf[Labelling[T]]
  }
}
