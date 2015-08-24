/*
 * Copyright (c) 2014 Miles Sabin
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

import coproduct.ExtendBy
import hlist.Prepend

object adjoin {
  /**
   * Type class supporting the "flattening" of either an `HList` or `Coproduct` in such a way that
   * `HList` (or `Coproduct`, in the `Coproduct` case) elements are flattened.
   *
   * @author Travis Brown
   */
  trait Adjoin[A] extends DepFn1[A] with Serializable

  trait LowPriorityAdjoin {
    type Aux[A, Out0] = Adjoin[A] { type Out = Out0 }

    implicit def hlistAdjoin0[H, T <: HList](implicit
      adjoinT: Adjoin[T] { type Out <: HList }
    ): Aux[H :: T, H :: adjoinT.Out] =
      new Adjoin[H :: T] {
        type Out = H :: adjoinT.Out

        def apply(a: H :: T): H :: adjoinT.Out = a.head :: adjoinT(a.tail)
      }

    implicit def coproductAdjoin0[H, T <: Coproduct](
      implicit adjoinT: Adjoin[T] { type Out <: Coproduct }
    ): Aux[H :+: T, H :+: adjoinT.Out] =
      new Adjoin[H :+: T] {
        type Out = H :+: adjoinT.Out

        def apply(a: H :+: T): H :+: adjoinT.Out = a match {
          case Inl(h) => Inl(h)
          case Inr(t) => Inr(adjoinT(t))
        }
      }
  }

  object Adjoin extends LowPriorityAdjoin {
    def apply[A](implicit adjoin: Adjoin[A]): Aux[A, adjoin.Out] = adjoin

    implicit def hnilAdjoin: Aux[HNil, HNil] = new Adjoin[HNil] {
      type Out = HNil

      def apply(a: HNil): HNil = a
    }

    implicit def cnilAdjoin: Aux[CNil, CNil] = new Adjoin[CNil] {
      type Out = CNil

      def apply(a: CNil): CNil = a
    }

    implicit def hlistLAdjoin1[H <: HList, T <: HList, OutT <: HList](implicit
      adjoinT: Aux[T, OutT],
      prepend: Prepend[H, OutT]
    ): Aux[H :: T, prepend.Out] =
      new Adjoin[H :: T] {
        type Out = prepend.Out

        def apply(a: H :: T): prepend.Out = prepend(a.head, adjoinT(a.tail))
      }

    implicit def coproductAdjoin1[H <: Coproduct, T <: Coproduct, OutT <: Coproduct](implicit
      adjoinT: Aux[T, OutT],
      extend: ExtendBy[H, OutT]
    ): Aux[H :+: T, extend.Out] =
      new Adjoin[H :+: T] {
        type Out = extend.Out

        def apply(a: H :+: T): extend.Out = a match {
          case Inl(h) => extend.right(h)
          case Inr(t) => extend.left(adjoinT(t))
        }
      }
  }
}
