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