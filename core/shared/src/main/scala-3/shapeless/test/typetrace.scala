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
package test

import scala.quoted.*

class TypeTrace[T]

object TypeTrace {
  transparent inline implicit def apply[T]: TypeTrace[T] = ${ TypeTraceMacros.applyImpl[T] }
}

object TypeTraceMacros {
  def applyImpl[T: Type](using Quotes): Expr[TypeTrace[T]] = {
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    report.info(s"Trace: ${tTpe.show} ${tTpe.dealias.show} ${tTpe.getClass.getName} ${tTpe.dealias.getClass.getName}")
    '{ new TypeTrace[T] }
  }
}
