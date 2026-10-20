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

trait CoproductScalaCompat {
  /**
   * Allows to specify a `Coproduct` type with a syntax similar to `Record` and `Union`, as follows,
   *
   * {{{
   * type ISB = Coproduct.`Int, String, Boolean`.T
   * }}}
   *
   * Literal types are allowed, so that the following is valid,
   *
   * {{{
   * type TwoTrueStr = Coproduct.`2, true, "str"`.T
   * }}}
   *
   * On Scala 3 type names are resolved as described for [[TypeCarrier]].
   */
  transparent inline def selectDynamic(inline tpeSelector: String): Any =
    ${ CoproductMacros.coproductType('tpeSelector) }
}

object CoproductMacros {
  def coproductType(tpeSelector: Expr[String])(using Quotes): Expr[Any] = {
    val elems = TypeParser.commaSeparated(tpeSelector.valueOrAbort).map(TypeParser.parseTypeOrAbort)
    TypeParser.carrier(TypeParser.refinement(List("T" -> MacroUtils.mkCoproductTpe(elems))))
  }
}
