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
 * Type class witnessing that the elements of L1 are a subset of the elements of L2
 *
 * @author Alois Cochard
 */
trait FilterNotRel[L1 <: HList, T, L2 <: HList]

object FilterNotRel {
  import TypeOperators._

  type *!=*[T] = {
    type λ[L1 <: HList, L2 <: HList] = FilterNotRel[L1, T, L2]
  }

  implicit def hnilFilterNot[T] = new FilterNotRel[HNil, T, HNil] {}                                                        
                                                                                                                                
  implicit def hlistFilterNot1[L <: HList, H, Out <: HList](implicit f : FilterNotRel[L, H, Out]) =
    new FilterNotRel[H :: L, H, Out] {}

  implicit def hlistFilterNot2[H, L <: HList, U, Out <: HList]
    (implicit f : FilterNotRel[L, U, Out], e: U =:!= H) =
      new FilterNotRel[H :: L, U, H :: Out] {}
}

/**
 * Type class witnessing that the elements of L1 are a subset of the elements of L2
 *
 * @author Alois Cochard
 */
trait FilterRel[L1 <: HList, T, L2 <: HList]

object FilterRel {
  type *==*[T] = {
    type λ[L1 <: HList, L2 <: HList] = FilterRel[L1, T, L2]
  }

  implicit def hnilFilter[T] = new FilterRel[HNil, T, HNil] {}                                                        
                                                                                                                                
  implicit def hlistFilter1[L <: HList, H, Out <: HList](implicit f : FilterRel[L, H, Out]) =
    new FilterRel[H :: L, H, H :: Out] {}

  implicit def hlistFilter2[H, L <: HList, U, Out <: HList](implicit f : FilterRel[L, U, Out]) = 
    new FilterRel[H :: L, U, Out] {}
}

/**
 * Type class witnessing that there is a natural transformation between two HLists
 *
 * @author Alois Cochard
 */
trait NatTRel[L1 <: HList, F[_], L2 <: HList, G[_]]

object NatTRel {
  import TypeOperators._

  type ~??>[F[_], G[_]] = {
    type λ[L1 <: HList, L2 <: HList] = NatTRel[L1, F, L2, G]
  }

  implicit def hnilNatT[F[_], G[_]] = new NatTRel[HNil, F, HNil, G] {}

  implicit def hlistNatT0[H, L1 <: HList, L2 <: HList, G[_]](implicit n : NatTRel[L1, Id, L2, G]) =
    new NatTRel[H :: L1, Id, G[H] :: L2, G] {}

  implicit def hlistNatT1[H, L1 <: HList, F[_], L2 <: HList](implicit n : NatTRel[L1, F, L2, Id]) =
    new NatTRel[F[H] :: L1, F, H :: L2, Id] {}

  implicit def hlistNatT2[H, L1 <: HList, F[_], L2 <: HList, G[_]](implicit n : NatTRel[L1, F, L2, G]) =
    new NatTRel[F[H] :: L1, F, G[H] :: L2, G] {}
}

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
