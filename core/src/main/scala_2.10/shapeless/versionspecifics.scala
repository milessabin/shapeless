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

import scala.collection.{ GenTraversableLike, GenTraversableOnce }
import scala.collection.generic.{ CanBuildFrom, IsTraversableLike }
import scala.collection.mutable.Builder

import scala.reflect.macros.whitebox

trait ScalaVersionSpecifics extends LP0 {
  private[shapeless] type BuildFrom[-F, -E, +T] = CanBuildFrom[F, E, T]
  private[shapeless] type Factory[-E, +T] = CanBuildFrom[Nothing, E, T]
  private[shapeless] type IsRegularIterable[Repr] = IsTraversableLike[Repr]
  private[shapeless] type LazyList[+T] = Stream[T]
  private[shapeless] type IterableOnce[+T] = GenTraversableOnce[T]
  private[shapeless] type IterableOps[T, CC[_], R] = GenTraversableLike[T, R]

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

  private[shapeless] implicit class EtaExpand(tpe: Type) {
    def etaExpand: Type = tpe
  }
}
