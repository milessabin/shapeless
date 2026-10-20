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

// Scala 3 doesn't infer the components of a tagged type `UI @@ TT` (an intersection) from it, so decompose it with a
// macro instead. See `Materialized` for why there are two macros

trait TagUnwrappedInstances extends TagUnwrappedInstances0 {
  transparent inline implicit def tagUnwrapped[T, UF]: Unwrapped.Aux[T, UF] = ${ UnwrappedMacros.tagUnwrapped[T, UF] }

  transparent inline implicit def materializeTagUnwrapped[T]: Materialized[Unwrapped[T]] =
    ${ UnwrappedMacros.materializedTagUnwrapped[T] }
}

trait TagUnwrappedInstances0 extends LowPriorityUnwrappedInstances {
  implicit def tagUnwrappedAux[T, UF](implicit unwrapped: Materialized[Unwrapped[T]], ev: unwrapped.value.U =:= UF): Unwrapped.Aux[T, UF] =
    ev.substituteCo[[u] =>> Unwrapped.Aux[T, u]](unwrapped.value)
}

object UnwrappedMacros {
  def tagUnwrapped[T: Type, UF: Type](using Quotes): Expr[Unwrapped.Aux[T, UF]] =
    mkTagUnwrapped[T].asInstanceOf[Expr[Unwrapped.Aux[T, UF]]]

  def materializedTagUnwrapped[T: Type](using Quotes): Expr[Materialized[Unwrapped[T]]] =
    MacroUtils.materialized(mkTagUnwrapped[T])

  def mkTagUnwrapped[T: Type](using Quotes): Expr[Unwrapped[T]] = {
    import quotes.reflect.*
    val taggedSym = TypeRepr.of[tag.Tagged].typeSymbol
    def parts(t: TypeRepr): List[TypeRepr] = t.dealias match {
      case AndType(l, r) => parts(l) ++ parts(r)
      case other => List(other)
    }
    val (tags, rest) = parts(TypeRepr.of[T]).partition(_.typeSymbol == taggedSym)
    if (tags.isEmpty || rest.isEmpty) report.errorAndAbort(s"${TypeRepr.of[T].show} is not a tagged type")
    val ui = rest.reduceLeft(AndType(_, _))
    val chain = Implicits.search(TypeRepr.of[Unwrapped].appliedTo(ui)) match {
      case s: ImplicitSearchSuccess => s.tree
      case f: ImplicitSearchFailure => report.errorAndAbort(f.explanation)
    }
    val uf = chain.tpe.widen match {
      case Refinement(_, "U", TypeBounds(u, _)) => u
      case other => other.select(other.typeSymbol.typeMember("U"))
    }
    val result = Refinement(TypeRepr.of[Unwrapped[T]], "U", TypeBounds(uf, uf))
    result.asType match {
      case '[r] => '{ ${ chain.asExpr }.asInstanceOf[r] }.asExprOf[Unwrapped[T]]
    }
  }
}

trait SelfUnwrappedScalaCompat { self: LowPriorityUnwrappedInstances =>
  // As general a type as `tagUnwrapped`'s, so that they are prioritized by their owners rather than their types
  implicit def selfUnwrapped[T, U](implicit ev: T =:= U): Unwrapped.Aux[T, U] =
    theSelfUnwrapped.asInstanceOf[Unwrapped.Aux[T, U]]
}
