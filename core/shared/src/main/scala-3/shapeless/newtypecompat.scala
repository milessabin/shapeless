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

trait NewtypeScalaCompat {
  /**
   * Implicit conversion of newtype to `Ops` type via an implicit companion object of `Ops`.
   *
   * On Scala 2 the companion object of a case class `Ops` is itself a function `Repr => Ops`, so publishing it as an
   * implicit value provides the conversion used by `newtypeOps`. On Scala 3 it isn't, so this conversion applies the
   * `apply` method of an implicit value of the companion's type instead.
   */
  implicit transparent inline def newtypeOpsViaCompanion[Repr, Ops](t: newtype.Newtype[Repr, Ops]): Ops =
    ${ NewtypeMacros.opsViaCompanion[Repr, Ops]('t) }
}

object NewtypeMacros {
  def opsViaCompanion[Repr: Type, Ops: Type](t: Expr[newtype.Newtype[Repr, Ops]])(using Quotes): Expr[Ops] = {
    import quotes.reflect.*
    val opsTpe = TypeRepr.of[Ops]
    val companion = opsTpe.typeSymbol.companionModule
    if (companion.isNoSymbol) report.errorAndAbort(s"${opsTpe.show} has no companion object")
    Implicits.search(companion.termRef) match {
      case s: ImplicitSearchSuccess =>
        val repr = '{ $t.asInstanceOf[Repr] }.asTerm
        val ops = Select.overloaded(s.tree, "apply", opsTpe.typeArgs, List(repr))
        if (!(ops.tpe <:< opsTpe)) report.errorAndAbort(s"${companion.name}.apply does not create a ${opsTpe.show}")
        ops.asExprOf[Ops]
      case f: ImplicitSearchFailure => report.errorAndAbort(f.explanation)
    }
  }
}
