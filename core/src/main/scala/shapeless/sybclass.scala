/*
 * Copyright (c) 2011-14 Miles Sabin
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

import scala.language.experimental.macros

import scala.reflect.macros.blackbox

import poly._

/**
 * An implementation of [http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/
 * "Scrap your boilerplate with class"] in Scala.
 *
 * @author Miles Sabin
 */

/**
 * Type class representing one-level generic queries.
 */
trait Data[F, T, R] {
  def gmapQ(t: T): List[R]
}

trait LowPriorityData {
  /**
   * Default Data type class instance.
   */
  implicit def dfltData[F, T, R]: Data[F, T, R] = new Data[F, T, R] {
    def gmapQ(t: T): List[R] = Nil
  }
}

object Data extends TypeClassCompanion with LowPriorityData {
  implicit def apply[P, T, R](implicit wd: Derive[Data[P, T, R]]): Data[P, T, R] = wd.instance

  def gmapQ[F, T, R](f: F)(t: T)(implicit data: Data[F, T, R]) = data.gmapQ(t)

  /**
   * Data type class instance for `List`s.
   */
  implicit def listData[P, T, R](implicit qt: Case1.Aux[P, T, R]): Data[P, List[T], R] = new Data[P, List[T], R] {
    def gmapQ(t: List[T]) = t.map(qt(_))
  }

  object typeClass extends MkTypeClass {
    implicit def apply[P, T, R] = unpack[Data[P, T, R], T](new DataTypeClass[P, R])
  }

  class DataTypeClass[P, R] extends TypeClass {
    type C[T] = Data[P, T, R]
    type Elem[T]  = Case1.Aux[P, T, R]

    def emptyProduct: Data[P, HNil, R] =
      new Data[P, HNil, R] {
        def gmapQ(t: HNil) = Nil
      }

    def product[H, T <: HList](qh: Case1.Aux[P, H, R], ct: Data[P, T, R]): Data[P, H :: T, R] =
      new Data[P, H :: T, R] {
        def gmapQ(t: H :: T) = qh(t.head :: HNil) :: ct.gmapQ(t.tail)
      }

    def emptyCoproduct: Data[P, CNil, R] =
      new Data[P, CNil, R] {
        def gmapQ(t: CNil) = Nil
      }

    def coproduct[H, T <: Coproduct](qh: => Case1.Aux[P, H, R], ct: => Data[P, T, R]): Data[P, H :+: T, R] =
      new Data[P, H :+: T, R] {
        def gmapQ(c: H :+: T) = c match {
          case Inl(h) => List(qh(h :: HNil))
          case Inr(t) => ct.gmapQ(t)
        }
      }

    def project[F, G](instance: => Data[P, G, R], to: F => G, from: G => F): Data[P, F, R] =
      new Data[P, F, R] {
        def gmapQ(t: F) = instance.gmapQ(to(t))
      }
  }
}

/**
 * Type class representing one-level generic transformations.
 */
trait DataT[F, T] {
  def gmapT(t: T): T
}

trait LowPriorityDataT {
  /**
   * Default DataT type class instance.
   */
  implicit def dfltDataT[F, T]: DataT[F, T] = new DataT[F, T] {
    def gmapT(t: T) = t
  }
}

object DataT extends TypeClassCompanion with LowPriorityDataT {
  implicit def apply[P, T](implicit wd: Derive[DataT[P, T]]): DataT[P, T] = wd.instance

  def gmapT[F, T](f: F)(t: T)(implicit data: DataT[F, T]) = data.gmapT(t)

  /**
   * DataT type class instance for `List`s.
   */
  implicit def listDataT[F <:  Poly, T](implicit ft: Case1.Aux[F, T, T]): DataT[F, List[T]] =
    new DataT[F, List[T]] {
      def gmapT(t: List[T]) = t.map(ft)
    }

  object typeClass extends MkTypeClass {
    implicit def apply[P, T] = unpack[DataT[P, T], T](new DataTTypeClass[P])
  }

  class DataTTypeClass[P] extends TypeClass {
    type C[T] = DataT[P, T]
    type Elem[T] = Case1.Aux[P, T, T]

    def emptyProduct: DataT[P, HNil] =
      new DataT[P, HNil] {
        def gmapT(t: HNil) = HNil
      }

    def product[H, T <: HList]
      (fh: Case1.Aux[P, H, H], ct: DataT[P, T]): DataT[P, H :: T] =
        new DataT[P, H :: T] {
          def gmapT(t: H :: T): H :: T = fh(t.head :: HNil) :: ct.gmapT(t.tail)
        }

    def emptyCoproduct: DataT[P, CNil] =
      new DataT[P, CNil] {
        def gmapT(t: CNil) = sys.error("CNil is equivelant to Nothing: there should be no values of this type")
      }

    def coproduct[H, T <: Coproduct]
      (fh: => Case1.Aux[P, H, H], ct: => DataT[P, T]): DataT[P, H :+: T] =
        new DataT[P, H :+: T] {
          def gmapT(c: H :+: T) = c match {
            case Inl(h) => Inl(fh(h :: HNil))
            case Inr(t) => Inr(ct.gmapT(t))
          }
        }

    def project[F, G](instance: => DataT[P, G], to: F => G, from: G => F): DataT[P, F] =
      new DataT[P, F] {
        def gmapT(t: F) = from(instance.gmapT(to(t)))
      }
  }
}

class EverythingAux[F, K] extends Poly

object EverythingAux {
  implicit def default[E, F <: Poly, K <: Poly, T, R]
    (implicit
      unpack: Unpack2[E, EverythingAux, F, K],
      f: Case1.Aux[F, T, R],
      data: Data[E, T, R],
      k: Case2.Aux[K, R, R, R]
    ): Case1.Aux[E, T, R] = Case1[E, T, R](t => data.gmapQ(t).foldLeft(f(t))(k))
}

class EverywhereAux[F] extends Poly

object EverywhereAux {
  implicit def default[E, F <: Poly, T]
    (implicit
      unpack: Unpack1[E, EverywhereAux, F],
      data: DataT.Derive[DataT[E, T]],
      f: Case1.Aux[F, T, T] = Case1[F, T, T](identity)
    ): Case1.Aux[E, T, T] = Case1[E, T, T](t => f(data.instance.gmapT(t)))
}
