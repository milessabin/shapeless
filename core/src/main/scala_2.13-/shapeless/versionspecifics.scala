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

object VersionSpecifics extends LP0 {
  type BuildFrom[-F, -E, +T] = CanBuildFrom[F, E, T]
  type Factory[-E, +T] = CanBuildFrom[Nothing, E, T]
  type IsIterableLike[Repr] = IsTraversableLike[Repr]
  type IterableLike[T, Repr] = GenTraversableLike[T, Repr]
  type LazyList[+T] = Stream[T]
  type IterableOnce[+T] = GenTraversableOnce[T]
  type IterableOps[T, CC[_], R] = GenTraversableLike[T, R]
  type Iterable[+T] = Traversable[T]
  type GenMap[K, +V] = scala.collection.GenMap[K, V]

  def implicitNotFoundMessage(c: whitebox.Context)(tpe: c.Type): String = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.typeSymbolDirect match {
      case global.analyzer.ImplicitNotFoundMsg(msg) =>
        msg.format(global.newTermName("evidence"), gTpe)
      case _ =>
        s"Implicit value of type $tpe not found"
    }
  }

  implicit class GenTraversableLikeOps[T, Repr](gtl: GenTraversableLike[T, Repr]) {
    def iterator: Iterator[T] = gtl.toIterator
  }

  implicit def canBuildFrom[F, E, T](cbf: CanBuildFrom[F, E, T]): CanBuildFromOps[F, E, T] =
    new CanBuildFromOps[F, E, T](cbf)
}

trait LP0 extends LP1 {
  implicit def canBuildFromNothing[E, T](cbf: CanBuildFrom[Nothing, E, T]): CanBuildFromOps[Nothing, E, T] =
    new CanBuildFromOps[Nothing, E, T](cbf)
}

trait LP1 {
  implicit def canBuildEmptyFromNothing[T](cbf: CanBuildFrom[Nothing, Nothing, T]): CanBuildFromOps[Nothing, Nothing, T] =
    new CanBuildFromOps[Nothing, Nothing, T](cbf)

  class CanBuildFromOps[F, E, T](cbf: CanBuildFrom[F, E, T]) {
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
