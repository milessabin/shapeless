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

import ops.hlist.{Selector => HSelector}
import ops.coproduct.{Selector => CSelector}

import scala.annotation.implicitNotFound

/**
 * Type class witnessing that every element of `L` has `TC` as its outer type constructor. 
 */
trait UnaryTCConstraint[L, TC[_]] extends Serializable

trait LowPriorityUnaryTCConstraint1 {
  implicit def idUnaryTC[T] = new UnaryTCConstraint[T, Id] {}
}

trait LowPriorityUnaryTCConstraint0 extends LowPriorityUnaryTCConstraint1 {
  implicit def hnilConstUnaryTC[H] = new UnaryTCConstraint[HNil, Const[H]#λ] {}
  implicit def hlistConstUnaryTC[H, T <: HList](implicit utct : UnaryTCConstraint[T, Const[H]#λ]) =
    new UnaryTCConstraint[H :: T, Const[H]#λ] {}

  implicit def cnilConstUnaryTC[H] = new UnaryTCConstraint[CNil, Const[H]#λ] {}
  implicit def coproductConstUnaryTC[H, T <: Coproduct](implicit utct: UnaryTCConstraint[T, Const[H]#λ]) =
    new UnaryTCConstraint[H :+: T, Const[H]#λ] {}
}

trait LowPriorityUnaryTCConstraint extends LowPriorityUnaryTCConstraint0{
  implicit def genericUnaryTC[G, L, TC[_]](implicit gen: Generic.Aux[G, L], utct: UnaryTCConstraint[L, TC]) =
    new UnaryTCConstraint[G, TC]{}
}

object UnaryTCConstraint extends LowPriorityUnaryTCConstraint {
  def apply[L, TC[_]](implicit utcc: UnaryTCConstraint[L, TC]): UnaryTCConstraint[L, TC] = utcc

  type *->*[TC[_]] = {
    type λ[L] = UnaryTCConstraint[L, TC] 
  }
  
  implicit def hnilUnaryTC[TC[_]] = new UnaryTCConstraint[HNil, TC] {}
  implicit def hlistUnaryTC[H, T <: HList, TC[_]](implicit utct : UnaryTCConstraint[T, TC]) =
    new UnaryTCConstraint[TC[H] :: T, TC] {}

  implicit def cnilUnaryTC[TC[_]] = new UnaryTCConstraint[CNil, TC]{}
  implicit def coproductUnaryTC[H, T <: Coproduct, TC[_]](implicit utct: UnaryTCConstraint[T, TC]) =
    new UnaryTCConstraint[TC[H] :+: T, TC] {}
}

/**
 * Type class witnessing that every element of `L` is an element of `M`.
 */
trait BasisConstraint[L, M] extends Serializable

trait LowPriorityBasisConstraint{
  implicit def genericBasis[L, M, LG, MG](implicit 
      genL: Generic.Aux[L, LG], 
      genM: Generic.Aux[M, MG], 
      bct: BasisConstraint[LG, MG]) =
    new BasisConstraint[L, M]{}
}

object BasisConstraint extends LowPriorityBasisConstraint{
  def apply[L, M](implicit bc: BasisConstraint[L, M]): BasisConstraint[L, M] = bc

  type Basis[M] = {
    type λ[L] = BasisConstraint[L, M] 
  }

  implicit def cnilBasis[M <: Coproduct] = new BasisConstraint[CNil, M]{}
  implicit def coproductBasis[H, T <: Coproduct, M <: Coproduct](implicit bct : BasisConstraint[T, M], sel : CSelector[M, H]) =
    new BasisConstraint[H :+: T, M] {}
  
  implicit def hnilBasis[M <: HList] = new BasisConstraint[HNil, M] {}
  implicit def hlistBasis[H, T <: HList, M <: HList](implicit bct : BasisConstraint[T, M], sel : HSelector[M, H]) =
    new BasisConstraint[H :: T, M] {}
}

/**
 * Type class witnessing that every element of `L` is a subtype of `B`.
 */
trait LUBConstraint[L, B] extends Serializable

trait LowPriorityLUBConstraint {
  implicit def genericLUB[G, L, B](implicit gen: Generic.Aux[G, L], bct: LUBConstraint[L, B]) =
    new LUBConstraint[G, B]{}
}

object LUBConstraint extends LowPriorityLUBConstraint {
  def apply[L, B](implicit lc: LUBConstraint[L, B]): LUBConstraint[L, B] = lc

  type <<:[B] = {
    type λ[L] = LUBConstraint[L, B] 
  }
  
  implicit def cnilLUB[T] = new LUBConstraint[CNil, T] {}
  implicit def coproductLUB[H, T <: Coproduct, B](implicit bct : LUBConstraint[T, B], ev: H <:< B) =
    new LUBConstraint[H :+: T, B] {}

  implicit def hnilLUB[T] = new LUBConstraint[HNil, T] {}
  implicit def hlistLUB[H, T <: HList, B](implicit bct : LUBConstraint[T, B], ev: H <:< B) =
    new LUBConstraint[H :: T, B] {}
}

/**
 * Type class witnessing that `L` doesn't contain elements of type `U`
 */
@implicitNotFound("Implicit not found: shapeless.NotContainsConstraint[${L}, ${U}]. ${L} contains an element of type ${U}.")
trait NotContainsConstraint[L, U] extends Serializable

trait LowPriorityNotContains {
  implicit def genericNotContains[G, L, U](implicit gen: Generic.Aux[G, L], nc: NotContainsConstraint[L, U]) =
    new NotContainsConstraint[G, U]{}
}

object NotContainsConstraint extends LowPriorityNotContains {
  def apply[L, U](implicit ncc: NotContainsConstraint[L, U]): NotContainsConstraint[L, U] = ncc

  type NotContains[U] = {
    type λ[L] = NotContainsConstraint[L, U]
  }

  implicit def cnilNotContains[U] = new NotContainsConstraint[CNil, U] {}
  implicit def coproductNotContains[H, T <: Coproduct, U](implicit nc: T NotContainsConstraint U, neq: U =:!= H) =
    new NotContainsConstraint[H :+: T, U] {}

  implicit def hnilNotContains[U] = new NotContainsConstraint[HNil, U] {}
  implicit def hlistNotContains[H, T <: HList, U](implicit nc: T NotContainsConstraint U, neq: U =:!= H) =
    new NotContainsConstraint[H :: T, U] {}
}

/**
 * Type class witnessing that all elements of `L` have distinct types
 */
@implicitNotFound("Implicit not found: shapeless.IsDistinctConstraint[${L}]. Some elements have the same type.")
trait IsDistinctConstraint[L] extends Serializable

trait LowPriorityIsDistinctConstraint {
  implicit def genericIsDistinct[G, L](implicit gen: Generic.Aux[G, L], d: IsDistinctConstraint[L]) =
    new IsDistinctConstraint[G]{}
}

object IsDistinctConstraint extends LowPriorityIsDistinctConstraint{
  def apply[L](implicit idc: IsDistinctConstraint[L]): IsDistinctConstraint[L] = idc

  implicit def cnilIsDistinct = new IsDistinctConstraint[CNil] {}
  implicit def coproductIsDistinct[H, T <: Coproduct](implicit d: IsDistinctConstraint[T],
                                                              nc: NotContainsConstraint[T, H]): IsDistinctConstraint[H :+: T] =
    new IsDistinctConstraint[H :+: T] {}

  implicit def hnilIsDistinct = new IsDistinctConstraint[HNil] {}
  implicit def hlistIsDistinct[H, T <: HList](implicit d: IsDistinctConstraint[T],
                                                      nc: NotContainsConstraint[T, H]): IsDistinctConstraint[H :: T] =
    new IsDistinctConstraint[H :: T] {}
}