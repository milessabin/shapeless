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
 * An implementation of [http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/ 
 * "Scrap your boilerplate with class"] in Scala.
 * 
 * @author Miles Sabin
 */
trait LowPrioritySybClass {
  import Poly._
  import Tuples._
  
  /**
   * Type class representing one-level generic queries.
   */
  trait Data[F, T, R] {
    def gmapQ(t : T) : List[R]
  }
  
  def gmapQ[F, T, R](f : F)(t : T)(implicit data : Data[F, T, R]) = data.gmapQ(t)

  /**
   * Default Data type class instance.
   */
  implicit def dfltData[F, T, R] = new Data[F, T, R] {
    def gmapQ(t : T) : List[R] = Nil
  }

  /**
   * Data type class instance for tuples.
   */
  implicit def tupleData[F <: Poly, T <: Product, L <: HList, R](implicit hl : HListerAux[T, L], dl : Data[F, L, R]) =
    new Data[F, T, R] {
      def gmapQ(t : T) = dl.gmapQ(t.hlisted)
    }

  /**
   * Data type class instance for `Either`.
   */
  implicit def eitherData[F <: Poly, T, U, R](implicit qt : Pullback1Aux[F, T, R], qu : Pullback1Aux[F, U, R]) =
    new Data[F, Either[T, U], R] {
      def gmapQ(t : Either[T, U]) = t match {
        case Left(t) => List(qt(t))
        case Right(u) => List(qu(u))
      }
    }

  /**
   * Data type class instance for `Option`.
   */
  implicit def optionData[F <: Poly, T, R](implicit qt : Pullback1Aux[F, T, R]) = new Data[F, Option[T], R] {
    def gmapQ(t : Option[T]) = t.map(qt).toList
  }

  /**
   * Data type class instance for `List`s.
   */
  implicit def listData[F <: Poly, T, R](implicit qt : Pullback1Aux[F, T, R]) = new Data[F, List[T], R] {
    def gmapQ(t : List[T]) = t.map(qt)
  }
  
  /**
   * Data type class instance for types with associated `Generic`s.
   */
  implicit def hlistIsoData[F <: Poly, T, L <: HList, R](implicit gen : GenericAux[T, L], dl : Data[F, L, R]) =
    new Data[F, T, R] {
      def gmapQ(t : T) = dl.gmapQ(gen.to(t))
    }
  
  /**
   * Data type class instance for `HList`s.
   */
  implicit def hnilData[F <: Poly, R] =
    new Data[F, HNil, R] {
      def gmapQ(t : HNil) = Nil
    }

  implicit def hlistData[F <: Poly, H, T <: HList, R](implicit qh : Pullback1Aux[F, H, R], ct : Data[F, T, R]) =
    new Data[F, H :: T, R] {
      def gmapQ(t : H :: T) = qh(t.head) :: ct.gmapQ(t.tail)
    }
  
  /**
   * Type class representing one-level generic transformations.
   */
  trait DataT[F, T] {
    def gmapT(t : T) : T
  }

  def gmapT[F, T](f : F)(t : T)(implicit data : DataT[F, T]) = data.gmapT(t)

  /**
   * Default DataT type class instance.
   */
  implicit def dfltDataT[F, T] : DataT[F, T] = new DataT[F, T] {
    def gmapT(t : T) = t
  }

  /**
   * DataT type class instance for tuples.
   */
  implicit def tupleDataT[F <: Poly, T <: Product, L <: HList]
    (implicit hl : HListerAux[T, L], tp : TuplerAux[L, T], dl : DataT[F, L]) =
      new DataT[F, T] {
        def gmapT(t : T) = dl.gmapT(t.hlisted).tupled
      }

  /**
   * DataT type class instance for `Either`.
   */
  implicit def eitherDataT[F <: Poly, T, U](implicit ft : HomAux[F, T], fu : HomAux[F, U]) =
    new DataT[F, Either[T, U]] {
      def gmapT(t : Either[T, U]) = t match {
        case Left(t) => Left(ft(t))
        case Right(u) => Right(fu(u))
      }
    }

  /**
   * DataT type class instance for `Option`.
   */
  implicit def optionDataT[F <: Poly, T](implicit ft : HomAux[F, T] ) = new DataT[F, Option[T]] {
    def gmapT(t : Option[T]) = t.map(ft)
  }

  /**
   * DataT type class instance for `List`s.
   */
  implicit def listDataT[F <:  Poly, T](implicit ft : HomAux[F, T]) = new DataT[F, List[T]] {
    def gmapT(t : List[T]) = t.map(ft)
  }
  
  /**
   * DataT type class instance for type with associated `Generics`s.
   */
  implicit def hlistIsoDataT[F <: Poly, T, L <: HList](implicit gen : GenericAux[T, L], dl : DataT[F, L]) =
    new DataT[F, T] {
      def gmapT(t : T) = gen.from(dl.gmapT(gen.to(t)))
    }

  /**
   * DataT type class instance for `HList`s.
   */
  implicit def hnilDataT[F <: Poly] =
    new DataT[F, HNil] {
      def gmapT(t : HNil) = HNil
    }

  implicit def hlistDataT[F <: Poly, H, T <: HList](implicit fh : HomAux[F, H], ct : DataT[F, T]) =
    new DataT[F, H :: T] {
      def gmapT(t : H :: T) = fh(t.head) :: ct.gmapT(t.tail)
    }

  /** The SYB everything combinator */
  type Everything[F <: Poly, K <: Poly, T] = Case1Aux[EverythingAux[F, K], T]
  
  class EverythingAux[F <: Poly, K <: Poly] extends Poly
  
  object EverythingAux {
    implicit def default[F <: Poly, K <: Poly, T, R]
      (implicit f : Pullback1Aux[F, T, R], data : Data[EverythingAux[F, K], T, R], k : Pullback2Aux[K, R, R, R]) =
        Case1Aux[EverythingAux[F, K], T, R](t => data.gmapQ(t).foldLeft(f(t))(k))
  }
  
  class ApplyEverything[F <: Poly] {
    def apply[K <: Poly](k : K) = new EverythingAux[F, K]
  }
  
  def everything[F <: Poly](f : F) = new ApplyEverything[F]

  /** The SYB everywhere combinator */
  type Everywhere[F <: Poly, T] = HomAux[EverywhereAux[F], T]

  class EverywhereAux[F <: Poly] extends Poly
  
  object EverywhereAux {
    implicit def default[F <: Poly, T](implicit data : DataT[EverywhereAux[F], T], f : HomAux[F, T]) =
      Case1Aux[EverywhereAux[F], T, T](t => f(data.gmapT(t)))
  }
  
  def everywhere[F <: Poly](f : F) = new EverywhereAux[f.type]
}

object SybClass extends LowPrioritySybClass {
  import Poly._
  
  implicit def hpHListData[F <: Poly, H, T <: HList, R](implicit qh : Pullback1Aux[F, H, R], ct : Data[F, T, R]) =
    new Data[F, H :: T, R] {
      def gmapQ(t : H :: T) = qh(t.head) :: ct.gmapQ(t.tail)
    }

  implicit def hpHListDataT[F <: Poly, H, T <: HList](implicit fh : HomAux[F, H], ct : DataT[F, T]) =
    new DataT[F, H :: T] {
      def gmapT(t : H :: T) = fh(t.head) :: ct.gmapT(t.tail)
    }
}
