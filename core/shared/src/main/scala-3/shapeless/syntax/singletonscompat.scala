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
package syntax

import scala.quoted.*

import labelled.KeyTag

trait SingletonOpsScalaCompat {
  type T

  /**
   * Returns the provided value tagged with the singleton type of this value as its key in a record-like structure.
   *
   * On Scala 3 this is a macro so that, as on Scala 2, the value type of the field is the type of the argument (eg. a
   * literal type for `2.narrow`), widened only if the argument is a literal.
   */
  transparent inline def ->>(inline v: Any): KeyTag[T, Any] = ${ SingletonOpsMacros.fieldImpl[T]('v) }
}

object SingletonOpsMacros {
  def fieldImpl[K: Type](v: Expr[Any])(using Quotes): Expr[KeyTag[K, Any]] = {
    import quotes.reflect.*
    val term = v.asTerm.underlyingArgument
    val vTpe = term match {
      case lit @ Literal(_) => lit.tpe.widen
      case other => MacroUtils.normalize(other.tpe.widenTermRefByName)
    }
    MacroUtils.mkFieldTpe(TypeRepr.of[K], vTpe).asType match {
      case '[f] => '{ $v.asInstanceOf[f] }.asExprOf[KeyTag[K, Any]]
    }
  }
}
