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
package ops

import VersionSpecifics._

object traversable {
  /**
   * Type class supporting type safe conversion of `Traversables` to `HLists`. 
   * 
   * @author Miles Sabin
   */
  trait FromTraversable[Out <: HList] extends Serializable {
    def apply(l : Iterable[_]) : Option[Out]
  }

  /**
   * `FromTraversable` type class instances.
   * 
   * @author Miles Sabin
   */
  object FromTraversable {
    def apply[Out <: HList](implicit from: FromTraversable[Out]) = from

    import syntax.typeable._

    implicit def hnilFromTraversable[T] = new FromTraversable[HNil] {
      def apply(l : Iterable[_]) =
        if(l.isEmpty) Some(HNil) else None 
    }
    
    implicit def hlistFromTraversable[OutH, OutT <: HList]
      (implicit flt : FromTraversable[OutT], oc : Typeable[OutH]) = new FromTraversable[OutH :: OutT] {
        def apply(l : Iterable[_]) : Option[OutH :: OutT] =
          if(l.isEmpty) None
          else for(h <- l.head.cast[OutH]; t <- flt(l.tail)) yield h :: t
    }
  }

  /**
   * Type class supporting type safe conversion of `Traversables` to `HLists` of a specific length.
   * 
   * @author Rob Norris
   */
  trait ToSizedHList[CC[_], A, N <: Nat] extends Serializable {
    type Out
    def apply(cc: CC[A]): Out
  }

  /**
   * `ToSizedHList` type class instances.
   * 
   * @author Rob Norris
   */
  object ToSizedHList {

    def apply[CC[_], A, N <: Nat](
      implicit ev: ToSizedHList[CC, A, N]
    ): ToSizedHList.Aux[CC, A, N, ev.Out] = 
      ev

    import syntax.sized._
    import ops.nat._
    import ops.sized._

    type Aux[CC[_], A, N <: Nat, Out0] =
      ToSizedHList[CC, A, N] {
        type Out = Out0
      }

    implicit def instance[CC[T] <: Iterable[T], A, N <: Nat](
      implicit gt: IsIterableLike[CC[A]],
               ac: AdditiveCollection[CC[A]],
               ti: ToInt[N], 
               th: ToHList[CC[A], N]
    ): Aux[CC, A, N, Option[th.Out]] =
      new ToSizedHList[CC, A, N] {
        type Out = Option[th.Out]
        def apply(as: CC[A]): Out =
          as.sized[N].map(_.toHList)
      }

  }

}
