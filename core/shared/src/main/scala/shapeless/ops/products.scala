/*
 * Copyright (c) 2013 Miles Sabin 
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

import hlist.Length

object product {
  trait ProductLength[T] extends DepFn1[T]

  object ProductLength {
    def apply[T](implicit length: ProductLength[T]): Aux[T, length.Out] = length

    type Aux[T, Out0] = ProductLength[T] { type Out = Out0 }
    
    implicit def length[T, L <: HList]
      (implicit gen: Generic.Aux[T, L], length: Length[L]): Aux[T, length.Out] =
        new ProductLength[T] {
          type Out = length.Out
          def apply(t: T): Out = length()
        }
  }
  
  trait ToTuple[P] extends DepFn1[P]
  
  object ToTuple {
    def apply[P](implicit toTuple: ToTuple[P]): Aux[P, toTuple.Out] = toTuple
    
    type Aux[P, Out0] = ToTuple[P] { type Out = Out0 }
    
    implicit def toTuple[P, L <: HList, T, Out0](implicit
      gen: Generic.Aux[P, L],
      tupler: ops.hlist.Tupler.Aux[L, T],
      ev: T <:< Out0                                                                
    ): Aux[P, Out0] = 
      new ToTuple[P] {
        type Out = Out0
        def apply(p: P) = ev(tupler(gen.to(p)))
      }
  }
  
  trait ToHList[P] extends DepFn1[P] { type Out <: HList }
  
  object ToHList {
    def apply[P](implicit toHList: ToHList[P]): Aux[P, toHList.Out] = toHList
    
    type Aux[P, Out0 <: HList] = ToHList[P] { type Out = Out0 }
    
    implicit def toHList[P, Out0 <: HList, L <: HList](implicit
      gen: Generic.Aux[P, L],
      ev: L <:< Out0                              
    ): Aux[P, Out0] =
      new ToHList[P] {
        type Out = Out0
        def apply(p: P) = ev(gen.to(p))
      }
  }

  trait ToRecord[P] extends DepFn1[P] { type Out <: HList }

  object ToRecord {
    def apply[P](implicit toRecord: ToRecord[P]): Aux[P, toRecord.Out] = toRecord

    type Aux[P, Out0 <: HList] = ToRecord[P] { type Out = Out0 }

    implicit def toRecord[P, Out0 <: HList, R <: HList](implicit
      lgen: LabelledGeneric.Aux[P, R],
      ev: R <:< Out0
    ): Aux[P, Out0] =
      new ToRecord[P] {
        type Out = Out0
        def apply(p: P) = ev(lgen.to(p))
      }
  }

  trait ToMap[P] extends DepFn1[P] {
    type K
    type V
    type Out = Map[K, V] 
  }

  object ToMap {
    def apply[P](implicit toMap: ToMap[P]): Aux[P, toMap.K, toMap.V] = toMap

    type Aux[P, K0, V0] = ToMap[P] { type K = K0; type V = V0 }

    implicit def productToMap[P, K0, V0, R <: HList](implicit
      lgen: LabelledGeneric.Aux[P, R], 
      toMap: ops.record.ToMap.Aux[R, K0, V0]
    ): Aux[P, K0, V0] =
      new ToMap[P] {
        type K = K0
        type V = V0
        def apply(p: P) = toMap(lgen.to(p))
      }
    
    implicit def emptyProductToMapNothing[P, K0](implicit
      lgen: LabelledGeneric.Aux[P, HNil],
      toMap: ops.record.ToMap.Aux[HNil, K0, Nothing]
    ): Aux[P, K0, Nothing] = productToMap[P, K0, Nothing, HNil]
  }

  trait ToTraversable[P, M[_]] extends DepFn1[P] {
    type Lub
    type Out = M[Lub]
  }

  object ToTraversable {
    def apply[P, M[_]](implicit toTraversable: ToTraversable[P, M]): Aux[P, M, toTraversable.Lub] = toTraversable

    type Aux[P, M[_], Lub0] = ToTraversable[P, M] { type Lub = Lub0 }

    implicit def productToTraversable[P, M[_], Lub0, L <: HList](implicit
      gen: Generic.Aux[P, L],
      toTraversable: ops.hlist.ToTraversable.Aux[L, M, Lub0]
    ): Aux[P, M, Lub0] =
      new ToTraversable[P, M] {
        type Lub = Lub0
        def apply(p: P) = toTraversable(gen.to(p))
      }

    implicit def emptyProductToTraversableNothing[P, M[_]](implicit
      gen: Generic.Aux[P, HNil],
      toTraversable: ops.hlist.ToTraversable.Aux[HNil, M, Nothing]
    ): Aux[P, M, Nothing] = productToTraversable[P, M, Nothing, HNil]
  }

  trait ToSized[P, M[_]] extends DepFn1[P] {
    type Lub
    type N <: Nat
    type Out = Sized[M[Lub], N]
  }

  object ToSized {
    def apply[P, M[_]](implicit toSized: ToSized[P, M]): Aux[P, M, toSized.Lub, toSized.N] = toSized

    type Aux[P, M[_], Lub0, N0 <: Nat] = ToSized[P, M] { type Lub = Lub0; type N = N0 }

    implicit def productToSized[P, M[_], Lub0, N0 <: Nat, L <: HList](implicit
      gen: Generic.Aux[P, L],
      toSized: ops.hlist.ToSized.Aux[L, M, Lub0, N0]
    ): Aux[P, M, Lub0, N0] =
      new ToSized[P, M] {
        type Lub = Lub0
        type N = N0
        def apply(p: P) = toSized(gen.to(p))
      }

    implicit def emptyProductToSizedNothing[P, M[_]](implicit
      gen: Generic.Aux[P, HNil],
      toSized: ops.hlist.ToSized.Aux[HNil, M, Nothing, _0]
    ): Aux[P, M, Nothing, _0] = productToSized[P, M, Nothing, _0, HNil]
  }
}
