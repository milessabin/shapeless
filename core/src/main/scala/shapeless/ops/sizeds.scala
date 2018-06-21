/*
 * Copyright (c) 2016-18 Miles Sabin
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
package ops

import VersionSpecifics._

object sized {
  /**
   * Type class supporting conversion of this `Sized` to an `HList` whose elements have the same type as in `Repr`.
   *
   * @author Alexandre Archambault
   */
  trait ToHList[-Repr, L <: Nat] extends Serializable {
    type Out <: HList
    def apply(s: Sized[Repr, L]): Out
  }

  object ToHList {
    type Aux[Repr, L <: Nat, Out0 <: HList] = ToHList[Repr, L] { type Out = Out0 }

    implicit val emptySizedToHList: Aux[Any, Nat._0, HNil] =
      new ToHList[Any, Nat._0] {
        type Out = HNil
        def apply(s: Sized[Any, Nat._0]) = HNil
      }

    implicit def nonEmptySizedToHList[Repr, L <: Nat]
      (implicit itl: IsIterableLike[Repr], ev: AdditiveCollection[Repr], ts: ToHList[Repr, L]): Aux[Repr, Succ[L], itl.A :: ts.Out] =
        new ToHList[Repr, Succ[L]] {
          type Out = itl.A :: ts.Out
          def apply(s: Sized[Repr, Succ[L]]) = s.head :: ts(s.tail)
        }
  }
}
