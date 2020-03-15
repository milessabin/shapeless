/*
 * Copyright (c) 2015 Miles Sabin
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

package shapeless.examples

import shapeless._

package FunctorDemoDefns {
  case class Foo[T](t: T, ts: List[T])

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]
}

object FunctorDemo extends App {
  import FunctorDemoDefns._
  import functorSyntax._

  def transform[F[_]: Functor, A, B](ft: F[A])(f: A => B): F[B] = ft.map(f)

  // Option has a Functor
  val o = transform(Option("foo"))(_.length)
  assert(o == Some(3))

  // List has a Functor
  val l = transform(List("foo", "wibble", "quux"))(_.length)
  assert(l == List(3, 6, 4))

  // Any case class has a Functor
  val foo = Foo("Three", List("French", "Hens"))

  val f0 = transform(foo)(_.length)
  val f1 = foo.map(_.length)           // they also have Functor syntax ...

  val expectedFoo = Foo(5, List(6, 4))
  assert(f0 == expectedFoo)
  assert(f1 == expectedFoo)

  // Any ADT has a Functor ... even with recursion
  val tree =
    Node(
      Leaf("quux"),
      Node(
        Leaf("foo"),
        Leaf("wibble")
      )
    )

  val t0 = transform(tree)(_.length)
  val t1 = tree.map(_.length)          // they also have Functor syntax ...

  val expectedTree =
    Node(
      Leaf(4),
      Node(
        Leaf(3),
        Leaf(6)
      )
    )
  assert(t0 == expectedTree)
  assert(t1 == expectedTree)
}

/**
 * Illustrative subset of the Cats Functor type class
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor extends Functor0 {
  def apply[F[_]](implicit f: Lazy[Functor[F]]): Functor[F] = f.value

  implicit val idFunctor: Functor[Id] =
    new Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }

  // Induction step for products
  implicit def hcons[F[_]](implicit ihc: IsHCons1[F, Functor, Functor]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = {
        val (hd, tl) = ihc.unpack(fa)
        ihc.pack((ihc.fh.map(hd)(f), ihc.ft.map(tl)(f)))
      }
    }

  implicit def constFunctor[T]: Functor[Const[T]#λ] =
    new Functor[Const[T]#λ] {
      def map[A, B](t: T)(f: A => B): T = t
    }
}

trait Functor0 {
  // Induction step for coproducts
  implicit def ccons[F[_]](implicit icc: IsCCons1[F, Functor, Functor]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        icc.pack(icc.unpack(fa).fold(hd => Left(icc.fh.map(hd)(f)), tl => Right(icc.ft.map(tl)(f))))
    }

  implicit def generic[F[_]](implicit gen: Generic1[F, Functor]): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        gen.from(gen.fr.map(gen.to(fa))(f))
    }
}

// Functor syntax
object functorSyntax {
  implicit def apply[F[_]: Functor, A](fa: F[A]): FunctorOps[F, A] =
    new FunctorOps[F, A](fa)

  class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(fa)(f)
  }
}
