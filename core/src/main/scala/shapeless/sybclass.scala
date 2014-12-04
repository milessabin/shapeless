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

trait Data0 {
  /**
   * Default Data type class instance.
   */
  implicit def dfltData[F, T, R]: Data[F, T, R] = new Data[F, T, R] {
    def gmapQ(t: T): List[R] = Nil
  }
}

object Data extends Data0 {
  def apply[P, T, R](implicit dt: Lazy[Data[P, T, R]]): Data[P, T, R] = dt.value

  def gmapQ[F, T, R](f: F)(t: T)(implicit data: Data[F, T, R]) = data.gmapQ(t)

  /**
   * Data type class instance for `List`s.
   */
  implicit def listData[P, T, R](implicit qt: Case1.Aux[P, T, R]): Data[P, List[T], R] = new Data[P, List[T], R] {
    def gmapQ(t: List[T]) = t.map(qt(_))
  }

  implicit def deriveHNil[P, R]: Data[P, HNil, R] =
    new Data[P, HNil, R] {
      def gmapQ(t: HNil) = Nil
    }

  implicit def deriveHCons[P, H, T <: HList, R]
    (implicit ch: Case1.Aux[P, H, R], dt: Lazy[Data[P, T, R]]): Data[P, H :: T, R] =
      new Data[P, H :: T, R] {
        def gmapQ(t: H :: T) = ch(t.head :: HNil) :: dt.value.gmapQ(t.tail)
      }

  implicit def deriveCNil[P, R]: Data[P, CNil, R] =
    new Data[P, CNil, R] {
      def gmapQ(t: CNil) = Nil
    }

  implicit def deriveCCons[P, H, T <: Coproduct, R]
    (implicit ch: Case1.Aux[P, H, R], dt: Lazy[Data[P, T, R]]): Data[P, H :+: T, R] =
      new Data[P, H :+: T, R] {
        def gmapQ(c: H :+: T) =
          c match {
            case Inl(h) => List(ch(h :: HNil))
            case Inr(t) => dt.value.gmapQ(t)
          }
      }

  implicit def deriveInstance[P, F, R, G](implicit gen: Generic.Aux[F, G], dg: Lazy[Data[P, G, R]]): Data[P, F, R] =
    new Data[P, F, R] {
      def gmapQ(t: F) = dg.value.gmapQ(gen.to(t))
    }
}

/**
 * Type class representing one-level generic transformations.
 */
trait DataT[F, T] {
  def gmapT(t: T): T
}

trait DataT0 {
  /**
   * Default DataT type class instance.
   */
  implicit def dfltDataT[F, T]: DataT[F, T] = new DataT[F, T] {
    def gmapT(t: T) = t
  }
}

object DataT extends DataT0 {
  def apply[P, T](implicit dtt: Lazy[DataT[P, T]]): DataT[P, T] = dtt.value

  def gmapT[F, T](f: F)(t: T)(implicit data: DataT[F, T]) = data.gmapT(t)

  /**
   * DataT type class instance for `List`s.
   */
  implicit def listDataT[F <:  Poly, T](implicit ft: Case1.Aux[F, T, T]): DataT[F, List[T]] =
    new DataT[F, List[T]] {
      def gmapT(t: List[T]) = t.map(ft)
    }

  implicit def deriveHNil[P]: DataT[P, HNil] =
    new DataT[P, HNil] {
      def gmapT(t: HNil) = HNil
    }

  implicit def deriveHCons[P, H, T <: HList]
    (implicit ch: Case1.Aux[P, H, H], dtt: Lazy[DataT[P, T]]): DataT[P, H :: T] =
      new DataT[P, H :: T] {
        def gmapT(t: H :: T): H :: T = ch(t.head :: HNil) :: dtt.value.gmapT(t.tail)
      }

  implicit def deriveCNil[P]: DataT[P, CNil] =
    new DataT[P, CNil] {
      def gmapT(t: CNil) = sys.error("CNil is equivelant to Nothing: there should be no values of this type")
    }

  implicit def deriveCCons[P, H, T <: Coproduct]
    (implicit ch: Case1.Aux[P, H, H], dtt: Lazy[DataT[P, T]]): DataT[P, H :+: T] =
      new DataT[P, H :+: T] {
        def gmapT(c: H :+: T) = c match {
          case Inl(h) => Inl(ch(h :: HNil))
          case Inr(t) => Inr(dtt.value.gmapT(t))
        }
      }

  implicit def deriveInstance[P, F, G](implicit gen: Generic.Aux[F, G], dtg: Lazy[DataT[P, G]]): DataT[P, F] =
    new DataT[P, F] {
      def gmapT(t: F) = gen.from(dtg.value.gmapT(gen.to(t)))
    }
}

class EverythingAux[F, K] extends Poly

object EverythingAux {
  implicit def default[E, F <: Poly, K <: Poly, T, R]
    (implicit
      unpack: Unpack2[E, EverythingAux, F, K],
      f: Case1.Aux[F, T, R],
      data: Lazy[Data[E, T, R]],
      k: Case2.Aux[K, R, R, R]
    ): Case1.Aux[E, T, R] = Case1[E, T, R](t => data.value.gmapQ(t).foldLeft(f(t))(k))
}

class EverywhereAux[F] extends Poly

object EverywhereAux {
  implicit def default[E, F <: Poly, T]
    (implicit
      unpack: Unpack1[E, EverywhereAux, F],
      data: Lazy[DataT[E, T]],
      f: Case1.Aux[F, T, T] = Case1[F, T, T](identity)
    ): Case1.Aux[E, T, T] = Case1[E, T, T](t => f(data.value.gmapT(t)))
}
