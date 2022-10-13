/*
 * Copyright (c) 2011-18 Miles Sabin
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

import scala.collection._

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
trait Data[F, T, R] extends Serializable {
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

trait Data1 extends Data0 {
  implicit def deriveInstance[P, F, R, G](implicit gen: Generic.Aux[F, G], dg: Lazy[Data[P, G, R]]): Data[P, F, R] =
    new Data[P, F, R] {
      def gmapQ(t: F) = dg.value.gmapQ(gen.to(t))
    }
}

object Data extends Data1 {
  def apply[P, T, R](implicit dt: Lazy[Data[P, T, R]]): Data[P, T, R] = dt.value

  def gmapQ[F, T, R](f: F)(t: T)(implicit data: Lazy[Data[F, T, R]]) = data.value.gmapQ(t)

  /**
   * Data type class instance for `List`s.
   */
  @deprecated("Superseded by genTraversableData", "2.3.1")
  def listData[P, T, R](implicit qt: Lazy[Case1.Aux[P, T, R]]): Data[P, List[T], R] =
    new Data[P, List[T], R] {
      def gmapQ(t: List[T]) = t.map(qt.value(_))
    }

  implicit def genTraversableData[P, C[X] <: Iterable[X], T, R]
    (implicit qt: Lazy[Case1.Aux[P, T, R]]): Data[P, C[T], R] =
      new Data[P, C[T], R] {
        def gmapQ(t: C[T]) =
          t.foldLeft(List.newBuilder[R]) { (b, el) =>
            b += qt.value(el)
          }.result()
      }

  implicit def genMapData[P, M[X, Y], K, V, R]
    (implicit ev: M[K, V] <:< Map[K, V], qv: Lazy[Case1.Aux[P, (K, V), R]]): Data[P, M[K, V], R] =
      new Data[P, M[K, V], R] {
        def gmapQ(t: M[K, V]) =
          t.foldLeft(List.newBuilder[R]) { case (b, el) =>
            b += qv.value(el)
          }.result()
      }

  implicit def deriveHNil[P, R]: Data[P, HNil, R] =
    new Data[P, HNil, R] {
      def gmapQ(t: HNil) = Nil
    }

  implicit def deriveHCons[P, H, T <: HList, R]
    (implicit ch: Lazy[Case1.Aux[P, H, R]], dt: Lazy[Data[P, T, R]]): Data[P, H :: T, R] =
      new Data[P, H :: T, R] {
        def gmapQ(t: H :: T) = ch.value(t.head :: HNil) :: dt.value.gmapQ(t.tail)
      }

  implicit def deriveCNil[P, R]: Data[P, CNil, R] =
    new Data[P, CNil, R] {
      def gmapQ(t: CNil) = Nil
    }

  implicit def deriveCCons[P, H, T <: Coproduct, R]
    (implicit ch: Lazy[Case1.Aux[P, H, R]], dt: Lazy[Data[P, T, R]]): Data[P, H :+: T, R] =
      new Data[P, H :+: T, R] {
        def gmapQ(c: H :+: T) =
          c match {
            case Inl(h) => List(ch.value(h :: HNil))
            case Inr(t) => dt.value.gmapQ(t)
          }
      }
}

/**
 * Type class representing one-level generic transformations.
 */
trait DataT[F, T] extends Serializable {
  type Out
  def gmapT(t: T): Out
}

trait DataT0 {
  type Aux[F, T, Out0] = DataT[F, T] { type Out = Out0 }

  /**
   * Default DataT type class instance.
   */
  implicit def dfltDataT[F, T]: Aux[F, T, T] = new DataT[F, T] {
    type Out = T
    def gmapT(t: T) = t
  }
}

trait DataT1 extends DataT0 {
  implicit def deriveInstance[P, F, G]
    (implicit gen: Generic.Aux[F, G], dtg: Lazy[DataT.Aux[P, G, G]]): Aux[P, F, F] =
      new DataT[P, F] {
        type Out = F
        def gmapT(t: F) = gen.from(dtg.value.gmapT(gen.to(t)))
      }
}

object DataT extends DataT1 {
  def apply[P, T](implicit dtt: Lazy[DataT[P, T]]): DataT[P, T] = dtt.value

  def gmapT[F, T](f: F)(t: T)(implicit data: Lazy[DataT[F, T]]) = data.value.gmapT(t)

  /**
   * DataT type class instance for `List`s.
   */
  @deprecated("Superseded by genTraversableDataT", "2.3.1")
  def listDataT[F <: Poly, T, U](implicit ft: Lazy[Case1.Aux[F, T, U]]): Aux[F, List[T], List[U]] =
    new DataT[F, List[T]] {
      type Out = List[U]
      def gmapT(t: List[T]) = t.map(ft.value)
    }

  implicit def genTraversableDataT[F <: Poly, CC[X] <: Iterable[X], T, U]
    (implicit ft: Lazy[Case1.Aux[F, T, U]], cbf: Factory[U, CC[U]]): Aux[F, CC[T], CC[U]] =
      new DataT[F, CC[T]] {
        type Out = CC[U]
        def gmapT(t: CC[T]) =
          t.foldLeft(cbf.newBuilder) { (b, x) =>
            b += ft.value(x)
          }.result()
      }

  implicit def genMapDataT[F <: Poly, M[X, Y], K, V, U]
    (implicit
      ev: M[K, V] <:< Map[K, V],
      fv: Lazy[Case1.Aux[F, V, U]],
      cbf: Factory[(K, U), M[K, U]]
    ): Aux[F, M[K, V], M[K, U]] =
      new DataT[F, M[K, V]] {
        type Out = M[K, U]
        def gmapT(t: M[K, V]) =
          t.foldLeft(cbf.newBuilder) { case (b, (k, v)) =>
            b += k -> fv.value(v)
          }.result()
      }

  implicit def deriveHNil[P]: Aux[P, HNil, HNil] =
    new DataT[P, HNil] {
      type Out = HNil
      def gmapT(t: HNil) = HNil
    }

  implicit def deriveHCons[P, H, T <: HList, OutH, OutT <: HList]
    (implicit ch: Lazy[Case1.Aux[P, H, OutH]], dtt: Lazy[DataT.Aux[P, T, OutT]]): Aux[P, H :: T, OutH :: OutT] =
      new DataT[P, H :: T] {
        type Out = OutH :: OutT
        def gmapT(t: H :: T): Out = ch.value(t.head :: HNil) :: dtt.value.gmapT(t.tail)
      }

  implicit def deriveCNil[P]: Aux[P, CNil, CNil] =
    new DataT[P, CNil] {
      type Out = CNil
      def gmapT(t: CNil) = sys.error("CNil is equivelant to Nothing: there should be no values of this type")
    }

  implicit def deriveCCons[P, H, T <: Coproduct, OutH, OutT <: Coproduct]
    (implicit ch: Lazy[Case1.Aux[P, H, OutH]], dtt: Lazy[DataT.Aux[P, T, OutT]]): Aux[P, H :+: T, OutH :+: OutT] =
      new DataT[P, H :+: T] {
        type Out = OutH :+: OutT
        def gmapT(c: H :+: T) = c match {
          case Inl(h) => Inl(ch.value(h :: HNil))
          case Inr(t) => Inr(dtt.value.gmapT(t))
        }
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
  implicit def default[E, F <: Poly, T, U, V]
    (implicit
      unpack: Unpack1[E, EverywhereAux, F],
      data: Lazy[DataT.Aux[E, T, U]],
      f: Case1.Aux[F, U, V] = Case1[F, U, U](identity)
    ): Case1.Aux[E, T, V] = Case1[E, T, V](t => f(data.value.gmapT(t)))
}
