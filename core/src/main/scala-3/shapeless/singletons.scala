/*
 * Copyright (c) 2013-16 Miles Sabin
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

import scala.quoted._

trait WidenScalaCompat {

  transparent inline given [T <: Singleton]: Widen[T] = ${WidenScalaCompat.widenImpl[T]}
}
object WidenScalaCompat {

  def widenImpl[T](using Quotes)(using tTpe: Type[T]): Expr[Widen[T]] = {
    /*
    import quotes.reflect.*
    val widenType = TypeRepr.of[T].dealias.widenTermRefByName.widen.asType
    widenType.asInstanceOf[Type[_ >: T]] match {
      case '[out] => '{Widen.instance[T, out](t => t.asInstanceOf[out])}
    }
    */
    '{???}
  }
}
