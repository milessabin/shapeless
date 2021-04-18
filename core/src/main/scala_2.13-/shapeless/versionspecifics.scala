/*
 * Copyright (c) 2018 Miles Sabin
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

import shapeless.ops.hlist.ToSized

import scala.collection.{GenTraversableLike, GenTraversableOnce}
import scala.collection.generic.{CanBuildFrom, IsTraversableLike}
import scala.collection.mutable.Builder
import scala.reflect.macros.whitebox

trait ScalaVersionSpecifics extends LP0 {
  private[shapeless] type BuildFrom[-F, -E, +T] = CanBuildFrom[F, E, T]
  private[shapeless] type Factory[-E, +T] = CanBuildFrom[Nothing, E, T]
  private[shapeless] type IsRegularIterable[Repr] = IsTraversableLike[Repr]
  private[shapeless] type LazyList[+T] = Stream[T]
  private[shapeless] type IterableOnce[+T] = GenTraversableOnce[T]
  private[shapeless] type IterableOps[T, CC[_], R] = GenTraversableLike[T, R]
  private[shapeless] type TraversableOrIterable[+T] = scala.collection.Traversable[T]
  private[shapeless] type GenTraversableOrIterable[+T] = scala.collection.GenTraversable[T]

  private[shapeless] def implicitNotFoundMessage(c: whitebox.Context)(tpe: c.Type): String = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.typeSymbolDirect match {
      case global.analyzer.ImplicitNotFoundMsg(msg) =>
        msg.format(global.newTermName("evidence"), gTpe)
      case _ =>
        s"Implicit value of type $tpe not found"
    }
  }

  private[shapeless] implicit class GenTraversableLikeOps[T, Repr](gtl: GenTraversableLike[T, Repr]) {
    def iterator: Iterator[T] = gtl.toIterator
  }

  private[shapeless] implicit def canBuildFrom[F, E, T](cbf: CanBuildFrom[F, E, T]): CanBuildFromOps[F, E, T] =
    new CanBuildFromOps[F, E, T](cbf)

  private[shapeless] implicit class NewEither[A, B](e: Either[A, B]) {
    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = e match {
      case Right(b) => f(b)
      case _        => e.asInstanceOf[Either[A1, B1]]
    }

    def toOption: Option[B] = e match {
      case Right(b) => Some(b)
      case _ => None
    }
  }

  private[shapeless] implicit class NewLeft[A, B](l: Left[A, B]) {
    def value: A = l match {
      case Left(a) => a
    }
  }

  private[shapeless] implicit class NewRight[A, B](r: Right[A, B]) {
    def value: B = r match {
      case Right(b) => b
    }
  }

  private[shapeless] implicit class NewIsIterable0[A0, Repr](itl: IsRegularIterable[Repr] { type A = A0 }) {
    def apply(r: Repr): GenTraversableLike[A0, Repr] = itl.conversion(r)
  }
}

trait LP0 extends LP1 {
  private[shapeless] implicit def canBuildFromNothing[E, T](cbf: CanBuildFrom[Nothing, E, T]): CanBuildFromOps[Nothing, E, T] =
    new CanBuildFromOps[Nothing, E, T](cbf)

  private[shapeless] implicit class NewIsIterable1[Repr](val itl: IsRegularIterable[Repr]) {
    def apply(r: Repr): GenTraversableLike[_, Repr] = itl.conversion(r)
  }
}

trait LP1 {
  private[shapeless] implicit def canBuildEmptyFromNothing[T](cbf: CanBuildFrom[Nothing, Nothing, T]): CanBuildFromOps[Nothing, Nothing, T] =
    new CanBuildFromOps[Nothing, Nothing, T](cbf)

  private[shapeless] class CanBuildFromOps[F, E, T](cbf: CanBuildFrom[F, E, T]) {
    def newBuilder: Builder[E, T] = cbf()
    def newBuilder(f: F): Builder[E, T] = cbf(f)
    def fromSpecific(gto: GenTraversableOnce[E]): T = {
      val b = cbf()
      b ++= gto.toIterator
      b.result()
    }
  }
}

trait CaseClassMacrosVersionSpecifics { self: CaseClassMacros =>
  import c.universe._

  val varargTpt = tq"_root_.scala.collection.Seq"
  val varargTC = typeOf[scala.collection.Seq[_]].typeConstructor
}

private[shapeless] trait AdditiveCollectionVersionSpecific {
  implicit def indexedSeqAdditiveCollection[T]: AdditiveCollection[IndexedSeq[T]] =
    new AdditiveCollection[IndexedSeq[T]] {}
}

private[shapeless] trait SizedVersionSpecific {
  def apply[CC[_]]()(
    implicit cbf: Factory[Nothing, CC[Nothing]],
    ev: AdditiveCollection[CC[Nothing]]
  ): Sized[CC[Nothing], _0] =
    Sized.wrap[CC[Nothing], _0](cbf.newBuilder.result())
}

private[shapeless] trait SizedOpsVersionSpecific[A0, Repr, L <: Nat] { self: SizedOps[A0, Repr, L] =>

  /**
   * Append the argument collection to this collection. The resulting collection will be statically known to have
   * ''m+n'' elements.
   */
  def ++[B >: A0, That, M <: Nat](that: Sized[That, M])(
    implicit sum: ops.nat.Sum[L, M],
    cbf: CanBuildFrom[Repr, B, That],
    convThat: That => GenTraversableLike[B, That],
    ev: AdditiveCollection[That]
  ): Sized[That, sum.Out] =
    Sized.wrap[That, sum.Out](underlying ++ that.unsized)
}

private[shapeless] trait RepeatVersionSpecific[L <: HList] extends Serializable {
  type Out <: HList
  def apply(l: L): Out
}

private[shapeless] trait ToSizedVersionSpecific {
  implicit def hlistToSized[H1, H2, T <: HList, LT, L, N0 <: Nat, M[_]](
    implicit
    tts  : ToSized.Aux[H2 :: T, M, LT, N0],
    u    : Lub[H1, LT, L],
    tvs2 : M[LT] => GenTraversableLike[LT, M[LT]], // tvs2, tev, and tcbf are required for the call to map below
    tev  : AdditiveCollection[M[LT]],
    tcbf : CanBuildFrom[M[LT], L, M[L]],
    tvs  : M[L] => GenTraversableLike[L, M[L]], // tvs, cbf, and ev are required for the call to +: below
    cbf  : CanBuildFrom[M[L], L, M[L]],
    ev   : AdditiveCollection[M[L]]
  ): ToSized.Aux[H1 :: H2 :: T, M, L, Succ[N0]] =
    new ToSized[H1 :: H2 :: T, M] {
      type Lub = L
      type N = Succ[N0]
      def apply(l : H1 :: H2 :: T) = u.left(l.head) +: tts(l.tail).map(u.right)
    }
}
