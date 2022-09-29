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

/** Type class representing one-level generic queries. */
trait Data[F, T, R] extends Serializable {
  def gmapQ(t: T): List[R]
}

sealed abstract class DataDefault {
  def instance[F, T, R](f: T => List[R]): Data[F, T, R] = new Data[F, T, R] {
    def gmapQ(t: T): List[R] = f(t)
  }

  /** Default Data type class instance. */
  implicit def default[F, T, R]: Data[F, T, R] =
    instance(_ => Nil)
}

sealed abstract class DataGeneric extends DataDefault {
  implicit def deriveInstance[F, T, R, Q](
    implicit gen: Generic.Aux[T, R], data: => Data[F, R, Q]
  ): Data[F, T, Q] = instance { t =>
    data.gmapQ(gen.to(t))
  }
}

sealed abstract class DataCollections extends DataGeneric {
  implicit def deriveIterable[F, I[_], T, R](
    implicit ev: I[T] <:< Iterable[T], cse: => Case1.Aux[F, T, R]
  ): Data[F, I[T], R] = instance {
    _.foldLeft(List.newBuilder[R])(_ += cse(_)).result()
  }

  implicit def deriveMap[F, M[_, _], K, V, R](
    implicit ev: M[K, V] <:< Map[K, V], cse: => Case1.Aux[F, (K, V), R]
  ): Data[F, M[K, V], R] = instance {
    _.foldLeft(List.newBuilder[R])(_ += cse(_)).result()
  }
}

object Data extends DataCollections {
  def apply[F, T, R](implicit data: Data[F, T, R]): Data[F, T, R] = data
  def gmapQ[F, T, R](f: F)(t: T)(implicit data: Data[F, T, R]): List[R] = data.gmapQ(t)

  implicit def deriveHNil[F, R]: Data[F, HNil, R] =
    instance(_ => Nil)

  implicit def deriveHCons[F, H, T <: HList, R](
    implicit cse: => Case1.Aux[F, H, R], data: => Data[F, T, R]
  ): Data[F, H :: T, R] = instance {
    case h :: t => cse(h) :: data.gmapQ(t)
  }

  implicit def deriveCNil[F, R]: Data[F, CNil, R] =
    instance(_ => Nil)

  implicit def deriveCCons[F, H, T <: Coproduct, R](
    implicit cse: => Case1.Aux[F, H, R], data: => Data[F, T, R]
  ): Data[F, H :+: T, R] = instance {
    case Inl(h) => List(cse(h))
    case Inr(t) => data.gmapQ(t)
  }
}

/** Type class representing one-level generic transformations. */
trait DataT[F, T] extends Serializable {
  type Out
  def gmapT(t: T): Out
}

sealed abstract class DataTDefault {
  type Aux[F, T, O] = DataT[F, T] { type Out = O }

  def instance[F, T, O](f: T => O): Aux[F, T, O] = new DataT[F, T] {
    type Out = O
    def gmapT(t: T): Out = f(t)
  }

  /** Default DataT type class instance. */
  implicit def dfltDataT[F, T]: Aux[F, T, T] =
    instance(identity)
}

sealed abstract class DataTGeneric extends DataTDefault {
  implicit def deriveInstance[F, T, R](
    implicit gen: Generic.Aux[T, R], data: => Aux[F, R, R]
  ): Aux[F, T, T] = instance { t =>
    gen.from(data.gmapT(gen.to(t)))
  }
}

sealed abstract class DataTCollections extends DataTGeneric {
  implicit def deriveIterable[F <: Poly, I[_], T, O](
    implicit ev: I[T] <:< Iterable[T], cse: => Case1.Aux[F, T, O], factory: Factory[O, I[O]]
  ): Aux[F, I[T], I[O]] = instance {
    _.foldLeft(factory.newBuilder)(_ += cse(_)).result()
  }

  implicit def deriveMap[F <: Poly, M[_, _], K, V, O](
    implicit ev: M[K, V] <:< Map[K, V], cse: => Case1.Aux[F, V, O], factory: Factory[(K, O), M[K, O]]
  ): Aux[F, M[K, V], M[K, O]] = instance {
    _.foldLeft(factory.newBuilder) {
      case (b, (k, v)) => b += k -> cse(v)
    }.result()
  }
}

object DataT extends DataTCollections {
  def apply[F, T](implicit data: DataT[F, T]): Aux[F, T, data.Out] = data
  def gmapT[F, T](f: F)(t: T)(implicit data: DataT[F, T]): data.Out = data.gmapT(t)

  implicit def deriveHNil[F]: Aux[F, HNil, HNil] =
    instance(identity)

  implicit def deriveHCons[F, H, T <: HList, OH, OT <: HList](
    implicit cse: => Case1.Aux[F, H, OH], data: => Aux[F, T, OT]
  ): Aux[F, H :: T, OH :: OT] = instance {
    case h :: t => cse(h) :: data.gmapT(t)
  }

  implicit def deriveCNil[F]: Aux[F, CNil, CNil] =
    instance(identity)

  implicit def deriveCCons[F, H, T <: Coproduct, OH, OT <: Coproduct](
    implicit cse: => Case1.Aux[F, H, OH], data: => Aux[F, T, OT]
  ): Aux[F, H :+: T, OH :+: OT] = instance {
    case Inl(h) => Inl(cse(h))
    case Inr(t) => Inr(data.gmapT(t))
  }
}

class EverythingAux[F, K] extends Poly

object EverythingAux {
  implicit def default[F <: Poly, K <: Poly, T, R](
    implicit
    f: Case1.Aux[F, T, R],
    data: => Data[EverythingAux[F, K], T, R],
    k: Case2.Aux[K, R, R, R]
  ): Case1.Aux[EverythingAux[F, K], T, R] =
    Case1(t => data.gmapQ(t).foldLeft(f(t))(k))
}

class EverywhereAux[F] extends Poly

object EverywhereAux extends EverywhereAuxDefault {
  implicit def everywhere[F, T, U, O](
    implicit
    data: => DataT.Aux[EverywhereAux[F], T, U],
    f: Case1.Aux[F, U, O]
  ): Case1.Aux[EverywhereAux[F], T, O] =
    Case1(t => f(data.gmapT(t)))
}

sealed abstract class EverywhereAuxDefault {
  implicit def default[F, T, U](
    implicit data: => DataT.Aux[EverywhereAux[F], T, U]
  ): Case1.Aux[EverywhereAux[F], T, U] =
    Case1(data.gmapT)
}
