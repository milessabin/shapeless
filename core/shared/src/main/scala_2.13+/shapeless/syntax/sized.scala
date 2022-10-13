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

object sized {

  implicit def genTraversableSizedConv[Repr](cc : Repr)(
    implicit iil: IsRegularIterable[Repr],
    ev: AdditiveCollection[Repr]
  ): SizedConv[Repr] =
    new SizedConv[Repr](cc)
  
  implicit def stringSizedConv(s: String): SizedConv[String] =
    new SizedConv[String](s)
}

final class SizedConv[Repr](r: Repr)(implicit iil: IsRegularIterable[Repr], ev2: AdditiveCollection[Repr]) {
  import ops.nat._

  def checkSize[L <: Nat](implicit toInt: ToInt[L]): Boolean =
    iil(r).size == toInt()

  def sized[L <: Nat](implicit toInt: ToInt[L]): Option[Sized[Repr, L]] =
    Option.when(checkSize)(Sized.wrap(r))

  def sized(l: Nat)(implicit toInt: ToInt[l.N]): Option[Sized[Repr, l.N]] =
    Option.when(checkSize)(Sized.wrap(r))
    
  def ensureSized[L <: Nat](implicit toInt: ToInt[L]): Sized[Repr, L] = {
    assert(checkSize)
    Sized.wrap(r)
  }
}
