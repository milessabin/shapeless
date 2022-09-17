/*
 * Copyright (c) 2011-20 Miles Sabin 
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

import ops.hlist.{Selector => HSelector}
import ops.coproduct.{Selector => CSelector}

import scala.annotation.implicitNotFound

/**
 * Type class witnessing that every element of `L` has `TC` as its outer type constructor. 
 */
@implicitNotFound("Implicit not found: shapeless.UnaryTCConstraint[${L}, ${TC}]. Some element of ${L} does not have ${TC} as it's outer type constructor.")
trait UnaryTCConstraint[L, TC[_]] extends Serializable

trait LowPriorityUnaryTCConstraint {
  private val dummy =  new UnaryTCConstraint[Any, Id] { }
  protected def instance[A, TC[_]]: UnaryTCConstraint[A, TC] =
    dummy.asInstanceOf[UnaryTCConstraint[A, TC]]

  implicit def hnilUnaryTC[TC[_]]: UnaryTCConstraint[HNil, TC] = instance[HNil, TC]
  implicit def cnilUnaryTC[TC[_]]: UnaryTCConstraint[CNil, TC] = instance[CNil, TC]

  implicit def hlistUnaryTC[H, T <: HList, TC[_]](
    implicit utc : UnaryTCConstraint[T, TC]
  ): UnaryTCConstraint[TC[H] :: T, TC] = instance[TC[H] :: T, TC]

  implicit def coproductUnaryTC[H, T <: Coproduct, TC[_]](
    implicit utc : UnaryTCConstraint[T, TC]
  ): UnaryTCConstraint[TC[H] :+: T, TC] = instance[TC[H] :+: T, TC]

  implicit def genericUnaryTC[A, R, TC[_]](
    implicit gen: Generic.Aux[A, R], utc: UnaryTCConstraint[R, TC]
  ): UnaryTCConstraint[A, TC] = instance[A, TC]

  implicit def hnilConstUnaryTC[TC]: UnaryTCConstraint[HNil, Const[TC]#λ] = instance[HNil, Const[TC]#λ]
  implicit def cnilConstUnaryTC[TC]: UnaryTCConstraint[CNil, Const[TC]#λ] = instance[CNil, Const[TC]#λ]

  implicit def hlistConstUnaryTC[H, T <: HList](
    implicit utc : UnaryTCConstraint[T, Const[H]#λ]
  ): UnaryTCConstraint[H :: T, Const[H]#λ] = instance[H :: T, Const[H]#λ]

  implicit def coproductConstUnaryTC[H, T <: Coproduct](
    implicit utc : UnaryTCConstraint[T, Const[H]#λ]
  ): UnaryTCConstraint[H :+: T, Const[H]#λ] = instance[H :+: T, Const[H]#λ]

  implicit def genericConstUnaryTC[A, R, TC](
    implicit gen: Generic.Aux[A, R], utc: UnaryTCConstraint[R, Const[TC]#λ]
  ): UnaryTCConstraint[A, Const[TC]#λ] = instance[A, Const[TC]#λ]
}

object UnaryTCConstraint extends LowPriorityUnaryTCConstraint {
  def apply[A, TC[_]](implicit utc: UnaryTCConstraint[A, TC]): UnaryTCConstraint[A, TC] = utc

  type *->*[TC[_]] = {
    type λ[A] = UnaryTCConstraint[A, TC]
  }

  implicit def hlistIdUnaryTC[L <: HList]: UnaryTCConstraint[L, Id] = instance[L, Id]
  implicit def coproductIdUnaryTC[C <: Coproduct]: UnaryTCConstraint[C, Id] = instance[C, Id]
}

/**
 * Type class witnessing that every element of `L` is an element of `M`.
 */
@implicitNotFound("Implicit not found: shapeless.BasisConstraint[${L}, ${M}]. Some element of ${L} is not an element of ${M}.")
trait BasisConstraint[L, M] extends Serializable

object BasisConstraint {
  def apply[L, M](implicit bc: BasisConstraint[L, M]): BasisConstraint[L, M] = bc

  type Basis[M] = {
    type λ[L] = BasisConstraint[L, M] 
  }

  private val dummy = new BasisConstraint[Any, Any] { }
  protected def instance[L, M]: BasisConstraint[L, M] =
    dummy.asInstanceOf[BasisConstraint[L, M]]
  
  implicit def hnilBasis[M <: HList]: BasisConstraint[HNil, M] = instance[HNil, M]
  implicit def cnilBasis[M <: Coproduct]: BasisConstraint[CNil, M] = instance[CNil, M]

  implicit def hlistBasis[H, T <: HList, M <: HList](
    implicit bct: BasisConstraint[T, M], sel: HSelector[M, H]
  ): BasisConstraint[H :: T, M] = instance[H :: T, M]

  implicit def coproductBasis[H, T <: Coproduct, M <: Coproduct](
    implicit bct: BasisConstraint[T, M], sel: CSelector[M, H]
  ): BasisConstraint[H :+: T, M] = instance[H :+: T, M]

  implicit def genericBasis[L, M, LG, MG](
    implicit
    genL: Generic.Aux[L, LG],
    genM: Generic.Aux[M, MG],
    bct: BasisConstraint[LG, MG]
  ): BasisConstraint[L, M] = instance[L, M]
}

/**
 * Type class witnessing that every element of `L` is a subtype of `B`.
 */
@implicitNotFound("Implicit not found: shapeless.LUBConstraint[${L}, ${B}]. ${L} contains an element not a subtype of ${B}.")
trait LUBConstraint[L, B] extends Serializable

object LUBConstraint {
  def apply[L, B](implicit lc: LUBConstraint[L, B]): LUBConstraint[L, B] = lc

  type <<:[B] = {
    type λ[L] = LUBConstraint[L, B] 
  }

  private val dummy = new LUBConstraint[Any, Any] { }
  protected def instance[L, B]: LUBConstraint[L, B] =
    dummy.asInstanceOf[LUBConstraint[L, B]]

  implicit def genericLUB[G, L, B](
    implicit gen: Generic.Aux[G, L], bct: LUBConstraint[L, B]
  ): LUBConstraint[G, B] = instance[G, B]
  
  implicit def hnilLUB[T]: LUBConstraint[HNil, T] = instance[HNil, T]
  implicit def cnilLUB[T]: LUBConstraint[CNil, T] = instance[CNil, T]

  implicit def hlistLUB[H, T <: HList, B](
    implicit bct : LUBConstraint[T, B], ev: H <:< B
  ): LUBConstraint[H :: T, B] = instance[H :: T, B]

  implicit def coproductLUB[H, T <: Coproduct, B](
    implicit bct : LUBConstraint[T, B], ev: H <:< B
  ): LUBConstraint[H :+: T, B] = instance[H :+: T, B]
}

/**
 * Type class witnessing that every element of `L` is of the form `FieldType[K, V]` where `K` is an element of `M`.
 */
trait KeyConstraint[L, M] extends Serializable

object KeyConstraint {
  import labelled._

  def apply[L, M](implicit kc: KeyConstraint[L, M]): KeyConstraint[L, M] = kc

  type Keys[M] = {
    type λ[L] = KeyConstraint[L, M] 
  }

  private val dummy = new KeyConstraint[Any, Any] { }
  protected def instance[L, M]: KeyConstraint[L, M] =
    dummy.asInstanceOf[KeyConstraint[L, M]]

  implicit def genericKey[L, LG, M, MG](
    implicit
    genL: Generic.Aux[L, LG],
    genM: Generic.Aux[M, MG],
    kc: KeyConstraint[LG, MG]
  ): KeyConstraint[L, M] = instance[L, M]
  
  implicit def hnilKeys[M <: HList]: KeyConstraint[HNil, M] = instance[HNil, M]
  implicit def cnilKeys[C <: Coproduct]: KeyConstraint[CNil, C] = instance[CNil, C]

  implicit def hlistKeys[K, V, T <: HList, M <: HList](
    implicit bct: KeyConstraint[T, M], sel: HSelector[M, K]
  ): KeyConstraint[FieldType[K, V] :: T, M] = instance[FieldType[K, V] :: T, M]

  implicit def coproductKeys[K, V, T <: Coproduct, M <: Coproduct](
    implicit bct: KeyConstraint[T, M], sel: CSelector[M, K]
  ): KeyConstraint[FieldType[K, V] :+: T, M] = instance[FieldType[K, V] :+: T, M]
}

/**
 * Type class witnessing that every element of `L` is of the form `FieldType[K, V]` where `V` is an element of `M`.
 */
trait ValueConstraint[L, M] extends Serializable

object ValueConstraint {
  import labelled._

  def apply[L, M](implicit vc: ValueConstraint[L, M]): ValueConstraint[L, M] = vc

  type Values[M] = {
    type λ[L] = ValueConstraint[L, M] 
  }

  private val dummy = new ValueConstraint[Any, Any] { }
  protected def instance[L, M]: ValueConstraint[L, M] =
    dummy.asInstanceOf[ValueConstraint[L, M]]

  implicit def genericValue[L, LG, M, MG](
    implicit
    genL: Generic.Aux[L, LG],
    genM: Generic.Aux[M, MG],
    vc: ValueConstraint[LG, MG]
  ): ValueConstraint[L, M] = instance[L, M]
  
  implicit def hnilValues[M <: HList]: ValueConstraint[HNil, M] = instance[HNil, M]
  implicit def cnilValues[C <: Coproduct]: ValueConstraint[CNil, C] = instance[CNil, C]

  implicit def hlistValues[K, V, T <: HList, M <: HList](
    implicit bct: ValueConstraint[T, M], sel: HSelector[M, V]
  ): ValueConstraint[FieldType[K, V] :: T, M] = instance[FieldType[K, V] :: T, M]

  implicit def coproductValues[K, V, T <: Coproduct, M <: Coproduct](
    implicit bct: ValueConstraint[T, M], sel: CSelector[M, V]
  ): ValueConstraint[FieldType[K, V] :+: T, M] = instance[FieldType[K, V] :+: T, M]
}

/**
 * Type class witnessing that `L` doesn't contain elements of type `U`
 */
@implicitNotFound("Implicit not found: shapeless.NotContainsConstraint[${L}, ${U}]. ${L} already contains element of type ${U}.")
trait NotContainsConstraint[L, U] extends Serializable

object NotContainsConstraint {
  def apply[L, U](implicit ncc: NotContainsConstraint[L, U]): NotContainsConstraint[L, U] = ncc

  type NotContains[U] = {
    type λ[L] = NotContainsConstraint[L, U]
  }

  private val dummy = new NotContainsConstraint[Any, Any] { }
  protected def instance[L, U]: NotContainsConstraint[L, U] =
    dummy.asInstanceOf[NotContainsConstraint[L, U]]

  implicit def genericNotContains[G, L, U](
    implicit gen: Generic.Aux[G, L], nc: NotContainsConstraint[L, U]
  ): NotContainsConstraint[G, U] = instance[G, U]

  implicit def hnilNotContains[U]: NotContainsConstraint[HNil, U] = instance[HNil, U]
  implicit def cnilNotContains[U]: NotContainsConstraint[CNil, U] = instance[CNil, U]

  implicit def hlistNotContains[H, T <: HList, U](
    implicit nc: T NotContainsConstraint U, neq: U =:!= H
  ): NotContainsConstraint[H :: T, U] = instance[H :: T, U]

  implicit def coproductNotContains[H, T <: Coproduct, U](
    implicit nc: T NotContainsConstraint U, neq: U =:!= H
  ): NotContainsConstraint[H :+: T, U] = instance[H :+: T, U]
}

/**
 * Type class witnessing that all elements of `L` have distinct types
 */
@implicitNotFound("Implicit not found: shapeless.IsDistinctConstraint[${L}]. Some elements have the same type.")
trait IsDistinctConstraint[L] extends Serializable

object IsDistinctConstraint {
  def apply[L](implicit idc: IsDistinctConstraint[L]): IsDistinctConstraint[L] = idc

  private val dummy = new IsDistinctConstraint[Any] { }
  protected def instance[L]: IsDistinctConstraint[L] =
    dummy.asInstanceOf[IsDistinctConstraint[L]]

  implicit def genericIsDistinct[G, L](
    implicit gen: Generic.Aux[G, L], d: IsDistinctConstraint[L]
  ): IsDistinctConstraint[G] = instance[G]

  implicit def hnilIsDistinct: IsDistinctConstraint[HNil] = instance[HNil]
  implicit def cnilIsDistinct: IsDistinctConstraint[CNil] = instance[CNil]

  implicit def hlistIsDistinct[H, T <: HList](
    implicit d: IsDistinctConstraint[T], nc: NotContainsConstraint[T, H]
  ): IsDistinctConstraint[H :: T] = instance[H :: T]

  implicit def coproductIsDistinct[H, T <: Coproduct](
    implicit d: IsDistinctConstraint[T], nc: NotContainsConstraint[T, H]
  ): IsDistinctConstraint[H :+: T] = instance[H :+: T]
}
