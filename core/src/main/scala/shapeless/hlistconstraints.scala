/*
 * Copyright (c) 2011 Miles Sabin 
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

import ops.hlist.Selector

/**
 * Type class witnessing that every element of `L` has `TC` as its outer type constructor. 
 */
trait UnaryTCConstraint[L <: HList, TC[_]]

object UnaryTCConstraint {
  import TypeOperators._
  
  type *->*[TC[_]] = {
    type λ[L <: HList] = UnaryTCConstraint[L, TC] 
  } 
  
  implicit def hnilUnaryTC[TC[_]] = new UnaryTCConstraint[HNil, TC] {}
  implicit def hlistUnaryTC1[H, T <: HList, TC[_]](implicit utct : UnaryTCConstraint[T, TC]) =
    new UnaryTCConstraint[TC[H] :: T, TC] {}
  
  implicit def hlistUnaryTC2[L <: HList] = new UnaryTCConstraint[L, Id] {}
  
  implicit def hlistUnaryTC3[H] = new UnaryTCConstraint[HNil, Const[H]#λ] {}
  implicit def hlistUnaryTC4[H, T <: HList](implicit utct : UnaryTCConstraint[T, Const[H]#λ]) =
    new UnaryTCConstraint[H :: T, Const[H]#λ] {}
}

/**
 * Type class witnessing that every element of `L` is an element of `M`.
 */
trait BasisConstraint[L <: HList, M <: HList]

object BasisConstraint {
  type Basis[M <: HList] = {
    type λ[L <: HList] = BasisConstraint[L, M] 
  } 
  
  implicit def hnilBasis[M <: HList] = new BasisConstraint[HNil, M] {}
  implicit def hlistBasis[H, T <: HList, M <: HList](implicit bct : BasisConstraint[T, M], sel : Selector[M, H]) =
    new BasisConstraint[H :: T, M] {}
}

object SuperConstraint {
  import BasisConstraint._

  type IsSuperOf[M <: HList, L <: HList] = BasisConstraint[M, L]

  trait SuperRelation[N <: HList, M <: HList, L <: HList]

  type Super[M <: HList] = {
    type λ[L <: HList] = M IsSuperOf L
  }

/**
 * If a BasisConstraint for [M, L] and [N, M] are given, then there should exist one for [N, L].
 * In a function which has proven [M, L] one can call another function which needs evidence for [N, L] when wrapped with this.
 * i.e. def acceptM[L <: HList: Super[M]#λ](l: L) = { prove { acceptN(l)(_: N IsSuperOf L) } }
 */
  def prove[E, H <: HList, K <: HList, J <: HList](f: H IsSuperOf K => E)(implicit e2: J IsSuperOf K, e: SuperRelation[H, J, K]): E = f(new (H IsSuperOf K) {})

  implicit def hlistSuperRelation[N <: HList, M <: HList, L <: HList](implicit e2: N IsSuperOf M, e1: M IsSuperOf L) = new SuperRelation[N, M, L] {}
}

/**
 * Type class witnessing that every element of `L` is a subtype of `B`.
 */
trait LUBConstraint[L <: HList, B]

object LUBConstraint {
  type <<:[B] = {
    type λ[L <: HList] = LUBConstraint[L, B] 
  } 
  
  implicit def hnilLUB[T] = new LUBConstraint[HNil, T] {}
  implicit def hlistLUB[H, T <: HList, B](implicit bct : LUBConstraint[T, B], ev: H <:< B) =
    new LUBConstraint[H :: T, B] {}
}

/**
 * Type class witnessing that every element of L is of the form FieldEntry[F] where F is an element of `M`.
 */
trait KeyConstraint[L <: HList, M <: HList]

object KeyConstraint {
  type Keys[M <: HList] = {
    type λ[L <: HList] = KeyConstraint[L, M] 
  }
  
  implicit def hnilKeys[M <: HList] = new KeyConstraint[HNil, M] {}
  implicit def hlistKeys[F <: FieldAux, V, T <: HList, M <: HList]
    (implicit bct : KeyConstraint[T, M], sel : Selector[M, F]) = new KeyConstraint[(F, V) :: T, M] {}
}

/**
 * Type class witnessing that every element of L is of the form FieldEntry[F] where F#valueType is an element of `M`.  
 */
trait ValueConstraint[L <: HList, M <: HList]

object ValueConstraint {
  type Values[M <: HList] = {
    type λ[L <: HList] = ValueConstraint[L, M] 
  }
  
  implicit def hnilValues[M <: HList] = new ValueConstraint[HNil, M] {}
  implicit def hlistValues[F <: FieldAux, V, T <: HList, M <: HList]
    (implicit bct : ValueConstraint[T, M], sel : Selector[M, V]) = new ValueConstraint[(F, V) :: T, M] {}
}
