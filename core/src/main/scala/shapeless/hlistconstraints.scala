/*
 * Copyright (c) 2011-15 Miles Sabin 
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

import shapeless.ops.hlist.Selector

import scala.annotation.implicitNotFound

/**
 * Type class witnessing that every element of `L` has `TC` as its outer type constructor. 
 */
trait UnaryTCConstraint[L <: HList, TC[_]] extends Serializable

trait LowPriorityUnaryTCConstraint0 {
  implicit def hlistIdUnaryTC[L <: HList]: UnaryTCConstraint[L, Id] =
    new UnaryTCConstraint[L, Id] {}
}

trait LowPriorityUnaryTCConstraint extends LowPriorityUnaryTCConstraint0 {

  def hnilConstUnaryTC[H]: UnaryTCConstraint[HNil, Const[H]#λ] =
    new UnaryTCConstraint[HNil, Const[H]#λ] {}

  implicit def hnilUnaryTC[TC[_]]: UnaryTCConstraint[HNil, TC] =
    new UnaryTCConstraint[HNil, TC] {}

  implicit def hlistConstUnaryTC[H, T <: HList](
    implicit utct: UnaryTCConstraint[T, Const[H]#λ]
  ): UnaryTCConstraint[H :: T, Const[H]#λ] =
    new UnaryTCConstraint[H :: T, Const[H]#λ] {}
}

object UnaryTCConstraint extends LowPriorityUnaryTCConstraint {
  def apply[L <: HList, TC[_]](implicit utcc: UnaryTCConstraint[L, TC]): UnaryTCConstraint[L, TC] = utcc

  type *->*[TC[_]] = {
    type λ[L <: HList] = UnaryTCConstraint[L, TC] 
  }

  implicit override def hnilConstUnaryTC[H]: UnaryTCConstraint[HNil, Const[H]#λ] =
    super.hnilConstUnaryTC[H]
  
  implicit def hlistUnaryTC[H, T <: HList, TC[_]](
    implicit utct: UnaryTCConstraint[T, TC]
  ): UnaryTCConstraint[TC[H] :: T, TC] =
    new UnaryTCConstraint[TC[H] :: T, TC] {}
}

/**
 * Type class witnessing that every element of `L` is an element of `M`.
 */
trait BasisConstraint[L <: HList, M <: HList] extends Serializable

object BasisConstraint {
  def apply[L <: HList, M <: HList](implicit bc: BasisConstraint[L, M]): BasisConstraint[L, M] = bc

  type Basis[M <: HList] = {
    type λ[L <: HList] = BasisConstraint[L, M] 
  } 
  
  implicit def hnilBasis[M <: HList] = new BasisConstraint[HNil, M] {}
  implicit def hlistBasis[H, T <: HList, M <: HList](implicit bct : BasisConstraint[T, M], sel : Selector[M, H]) =
    new BasisConstraint[H :: T, M] {}
}

/**
 * Type class witnessing that every element of `L` is a subtype of `B`.
 */
trait LUBConstraint[L <: HList, B] extends Serializable

object LUBConstraint {
  def apply[L <: HList, B](implicit lc: LUBConstraint[L, B]): LUBConstraint[L, B] = lc

  type <<:[B] = {
    type λ[L <: HList] = LUBConstraint[L, B] 
  } 
  
  implicit def hnilLUB[T] = new LUBConstraint[HNil, T] {}
  implicit def hlistLUB[H, T <: HList, B](implicit bct : LUBConstraint[T, B], ev: H <:< B) =
    new LUBConstraint[H :: T, B] {}
}

/**
 * Type class witnessing that every element of `L` is of the form `FieldType[K, V]` where `K` is an element of `M`.
 */
trait KeyConstraint[L <: HList, M <: HList] extends Serializable

object KeyConstraint {
  import labelled._

  def apply[L <: HList, M <: HList](implicit kc: KeyConstraint[L, M]): KeyConstraint[L, M] = kc

  type Keys[M <: HList] = {
    type λ[L <: HList] = KeyConstraint[L, M] 
  }
  
  implicit def hnilKeys[M <: HList] = new KeyConstraint[HNil, M] {}
  implicit def hlistKeys[K, V, T <: HList, M <: HList]
    (implicit bct : KeyConstraint[T, M], sel : Selector[M, K]) = new KeyConstraint[FieldType[K, V] :: T, M] {}
}

/**
 * Type class witnessing that every element of `L` is of the form `FieldType[K, V]` where `V` is an element of `M`.
 */
trait ValueConstraint[L <: HList, M <: HList] extends Serializable

object ValueConstraint {
  import labelled._

  def apply[L <: HList, M <: HList](implicit vc: ValueConstraint[L, M]): ValueConstraint[L, M] = vc

  type Values[M <: HList] = {
    type λ[L <: HList] = ValueConstraint[L, M] 
  }
  
  implicit def hnilValues[M <: HList] = new ValueConstraint[HNil, M] {}
  implicit def hlistValues[K, V, T <: HList, M <: HList]
    (implicit bct : ValueConstraint[T, M], sel : Selector[M, V]) = new ValueConstraint[FieldType[K, V] :: T, M] {}
}

/**
 * Type class witnessing that `L` doesn't contain elements of type `U`
 */
@implicitNotFound("Implicit not found: shapeless.NotContainsConstraint[${L}, ${U}]. This HList already contains element of type ${U}.")
trait NotContainsConstraint[L <: HList, U] extends Serializable

object NotContainsConstraint {

  def apply[L <: HList, U](implicit ncc: NotContainsConstraint[L, U]): NotContainsConstraint[L, U] = ncc

  type NotContains[U] = {
    type λ[L <: HList] = NotContainsConstraint[L, U]
  }

  implicit def hnilNotContains[U] = new NotContainsConstraint[HNil, U] {}
  implicit def hlistNotContains[H, T <: HList, U](implicit nc: T NotContainsConstraint U, neq: U =:!= H) =
    new NotContainsConstraint[H :: T, U] {}
}

/**
 * Type class witnessing that all elements of `L` have distinct types
 */
@implicitNotFound("Implicit not found: shapeless.IsDistinctConstraint[${L}]. Some elements have the same type.")
trait IsDistinctConstraint[L <: HList] extends Serializable

object IsDistinctConstraint {

  def apply[L <: HList](implicit idc: IsDistinctConstraint[L]): IsDistinctConstraint[L] = idc

  implicit def hnilIsDistinct = new IsDistinctConstraint[HNil] {}
  implicit def hlistIsDistinct[H, T <: HList](implicit d: IsDistinctConstraint[T],
                                                      nc: NotContainsConstraint[T, H]): IsDistinctConstraint[H :: T] =
    new IsDistinctConstraint[H :: T] {}
}
