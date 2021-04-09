/*
 * Copyright (c) 2011-18 Miles Sabin 
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

import scala.collection.{GenTraversable, GenTraversableLike}

object sized {

  implicit def genTraversableSizedConv[CC[X] <: GenTraversable[X], T](cc : CC[T])(
    implicit conv: CC[T] => GenTraversableLike[T, CC[T]],
    ev: AdditiveCollection[CC[T]]
  ): SizedConv[T, CC[T]] =
    new SizedConv[T, CC[T]](cc)

  implicit def stringSizedConv(s: String): SizedConv[Char, String] =
    new SizedConv[Char, String](s)
}

final class SizedConv[A, Repr <% GenTraversableLike[A, Repr] : AdditiveCollection](r: Repr) {
  import ops.nat._

  def sized[L <: Nat](implicit toInt: ToInt[L]): Option[Sized[Repr, L]] =
    if (r.size == toInt()) Some(Sized.wrap(r)) else None

  def sized(l: Nat)(implicit toInt: ToInt[l.N]): Option[Sized[Repr, l.N]] =
    if (r.size == toInt()) Some(Sized.wrap(r)) else None

  def ensureSized[L <: Nat](implicit toInt: ToInt[L]): Sized[Repr, L] = {
    assert(r.size == toInt())
    Sized.wrap(r)
  }
}
