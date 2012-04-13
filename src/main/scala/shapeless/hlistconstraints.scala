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
trait FilterNotRel[L <: HList, T] { type Out <: HList }

object FilterNotRel {
  type *!=*[T] = {
    type λ[L <: HList] = FilterNotRel[L, T]
  }

  implicit def hlistFilterNot[L <: HList, T, Out0 <: HList]
    (implicit aux : FilterNotRelAux[L, T, Out0]) = new FilterNotRel[L, T] { type Out = Out0 }
}

trait FilterNotRelAux[L <: HList, T, Out <: HList]

object FilterNotRelAux {
  import TypeOperators._

  implicit def hnilFilterNot[T] = new FilterNotRelAux[HNil, T, HNil] {}                                                        
                                                                                                                                
  implicit def hlistFilterNot1[L <: HList, H, Out <: HList](implicit f : FilterNotRelAux[L, H, Out]) =
    new FilterNotRelAux[H :: L, H, Out] {}

  implicit def hlistFilterNot2[H, L <: HList, U, Out <: HList]
    (implicit f : FilterNotRelAux[L, U, Out], e: U =:!= H) =
      new FilterNotRelAux[H :: L, U, H :: Out] {}
}

/**
 * Type class witnessing that the elements of L1 are a subset of the elements of L2
 *
 * @author Alois Cochard
 */
trait FilterRel[L <: HList, T] { type Out <: HList }

object FilterRel {
  type *==*[T] = {
    type λ[L <: HList] = FilterRel[L, T]
  }

  implicit def hlistFilter[L <: HList, T, Out0 <: HList]
    (implicit aux : FilterRelAux[L, T, Out0]) = new FilterRel[L, T] { type Out = Out0 }
}

trait FilterRelAux[L <: HList, T, Out <: HList]

object FilterRelAux {
  implicit def hnilFilter[T] = new FilterRelAux[HNil, T, HNil] {}
                                                                                                                                
  implicit def hlistFilter1[L <: HList, H, Out <: HList](implicit f : FilterRelAux[L, H, Out]) =
    new FilterRelAux[H :: L, H, H :: Out] {}

  implicit def hlistFilter2[H, L <: HList, U, Out <: HList](implicit f : FilterRelAux[L, U, Out]) = 
    new FilterRelAux[H :: L, U, Out] {}
}

/**
 * Type class witnessing that there is a natural transformation between two HLists
 *
 * @author Alois Cochard
 */
trait NatTRel[L <: HList, F[_], G[_]] {
  type Out <: HList
}

object NatTRel {
  import TypeOperators._

  type ~??>[F[_], G[_]] = {
    type λ[L <: HList] = NatTRel[L, F, G]
  }

  implicit def hlistNatT0[L <: HList, F[_], Out0 <: HList, G[_]]
    (implicit aux : NatTRelAux[L, F, Out0, G], e : G[_] =:!= Id[_]) = new NatTRel[L, F, G] { type Out = Out0 }

  implicit def hlistNatT1[L <: HList, Out0 <: HList, G[_]]
    (implicit aux : NatTRelAux[L, Id, Out0, G]) = new NatTRel[L, Id, G] { type Out = Out0 }

  implicit def hlistNatT2[L <: HList, F[_], Out0 <: HList]
    (implicit aux : NatTRelAux[L, F, Out0, Id]) = new NatTRel[L, F, Id] { type Out = Out0 }
}

trait NatTRelAux[L <: HList, F[_], Out <: HList, G[_]] 

object NatTRelAux {
  import TypeOperators._

  implicit def hnilNatT[F[_], G[_]] = new NatTRelAux[HNil, F, HNil, G] {}

  implicit def hlistNatT0[H, L <: HList, G[_], Out <: HList](implicit n : NatTRelAux[L, Id, Out, G]) =
    new NatTRelAux[H :: L, Id, G[H] :: Out,  G] {}

  implicit def hlistNatT1[H, L <: HList, F[_], Out <: HList](implicit n : NatTRelAux[L, F, Out, Id]) =
    new NatTRelAux[F[H] :: L, F, H :: Out, Id] {}

  implicit def hlistNatT2[H, L <: HList, F[_], G[_], Out <: HList]
    (implicit n : NatTRelAux[L, F, Out, G], e : G[_] =:!= Id[_]) =
      new NatTRelAux[F[H] :: L, F, G[H] :: Out, G] {}
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
