/*
 * Copyright (c) 2011-13 Miles Sabin
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

import scala.collection.generic.IsTraversableLike

object sized {
  /**
   * Type class supporting type safe conversion of `Sized`s to `HList`s.
   *
   * @author Travis Brown
   */
  trait FromSized[Repr, N <: Nat] extends DepFn1[Sized[Repr, N]] {
    type Out <: HList
  }

  /**
   * `FromSized` type class instances.
   *
   * @author Travis Brown
   */
  object FromSized {
    import shapeless.nat._0

    def apply[Repr, N <: Nat](implicit fs: FromSized[Repr, N]): Aux[Repr, N, fs.Out] = fs

    type Aux[Repr, N <: Nat, Out0 <: HList] = FromSized[Repr, N] { type Out = Out0 }

    implicit def hnilFromSized[Repr]: Aux[Repr, _0, HNil] = new FromSized[Repr, _0] {
      type Out = HNil
      def apply(sized: Sized[Repr, _0]) = HNil
    }

    implicit def hlistFromSized[Repr, M <: Nat, OutT <: HList]
      (implicit fs: Aux[Repr, M, OutT], itl: IsTraversableLike[Repr]): Aux[Repr, Succ[M], itl.A :: OutT] = new FromSized[Repr, Succ[M]] {
        type Out = itl.A :: OutT
        def apply(sized: Sized[Repr, Succ[M]]) = sized.head :: fs(sized.tail)
    }
  }
}
