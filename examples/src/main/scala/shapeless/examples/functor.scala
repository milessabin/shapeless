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
import ops.coproduct.IsCCons
import ops.hlist.IsHCons

package FunctorDemoDefns {
  case class Foo[T](t: T, ts: List[T])

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]
}

object FunctorDemo extends App {
  import Functor._
  import FunctorDemoDefns._

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

object Functor {
  def apply[F[_]](implicit f: Functor[F]): Functor[F] = f

  implicit def constFunctor[T]: Functor[Const[T]#λ] =
    new Functor[Const[T]#λ] {
      def map[A, B](t: T)(f: A => B): T = t
    }

  implicit val idFunctor: Functor[Id] =
    new Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }

  // The functor for F[_] is derived indirectly by deriving an instance of
  // FunctorA[F[Arbitrary]]. Deriving for a type of kind * allows us to avoid
  // SI-2712 (https://issues.scala-lang.org/browse/SI-2712) related issues
  // with higher kinded unification.
  implicit def mkFunctor[F[_], R[_]]
    (implicit
      gen: Generic1.Aux[F, R],
      far: Lazy[FunctorA[R[FunctorA.Arbitrary]] { type F[t] = R[t] }]
    ): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        gen.from(far.value.map(gen.to(fa))(f))
    }

  // Functor syntax
  implicit def functorSyntax[F[_]: Functor, A](fa: F[A]): FunctorOps[F, A] =
    new FunctorOps[F, A](fa)

  class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(fa)(f)
  }
}

/**
 * Generic derivation of the functor F[_] instantiated at an "arbitrary"
 * type. This is an idea borrowed from the logician and philospher Kit Fine
 * (see his "Reasoning with Arbitrary Objects") and is related to Hilbert's
 * Epsilon calculus (http://en.wikipedia.org/wiki/Epsilon_calculus).
 *
 * The gist of the idea is that we encode universal quantification, (Ax)Fx,
 * as predication to an "arbitrary" object, Farbitrary. A simple example is
 * the encoding of "All triangles have three sides" as "The arbitrary
 * triangle has three sides". It's perhaps a little surprising, but full
 * first order predicate can be encoded in this way.
 *
 * Aside from this curiousity, the derivation is a fairly standard
 * derivation in terms of a sum of products generic representation of
 * the target ADT. This is provided by shapeless's Generic1 generic
 * programming primitive. The rest of the work is done via standard
 * implicit-based structural recursion of the representation type
 * with shapeless's Lazy construct used to inhibit divergence.
 */
trait FunctorA[FA] {
  type F[_]
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object FunctorA extends FunctorA0 {
  def apply[FA](implicit f: Lazy[FunctorA[FA]]): Aux[FA, f.value.F] = f.value

  // Base case for deriving products
  implicit def hnil: FunctorA[HNil] { type F[t] = HNil } =
    new FunctorA[HNil] {
      type F[t] = HNil
      def map[A, B](fa: HNil)(f: A => B): HNil = fa
    }

  // Induction step for products
  implicit def hcons[HTA <: HList, HA, TA <: HList]
    (implicit
      ihc: IsHCons.Aux[HTA, HA, TA],
      fh: Lazy[FunctorA[HA]],
      ft: Lazy[FunctorA[TA] { type F[t] <: HList }]
    ): FunctorA[HTA] { type F[t] = fh.value.F[t] :: ft.value.F[t] } =
    new FunctorA[HTA] {
      type F[t] = fh.value.F[t] :: ft.value.F[t]
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        fh.value.map(fa.head)(f) :: ft.value.map(fa.tail)(f)
    }

  // Base case for deriving coproducts
  implicit def cnil: FunctorA[CNil] { type F[t] = CNil } =
    new FunctorA[CNil] {
      type F[t] = CNil
      def map[A, B](fa: CNil)(f: A => B): CNil = fa
    }

  // Induction step for coproducts
  implicit def ccons[HTA <: Coproduct, HA, TA <: Coproduct]
    (implicit
      icc: IsCCons.Aux[HTA, HA, TA],
      fh: Lazy[FunctorA[HA]],
      ft: Lazy[FunctorA[TA] { type F[t] <: Coproduct }]
    ): FunctorA[HTA] { type F[t] = fh.value.F[t] :+: ft.value.F[t] } =
    new FunctorA[HTA] {
      type F[t] = fh.value.F[t] :+: ft.value.F[t]
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        fa match {
          case Inl(ha) => Inl(fh.value.map(ha)(f))
          case Inr(ta) => Inr(ft.value.map(ta)(f))
        }
    }
}

trait FunctorA0 extends FunctorA1 {
  // Case for atoms for which a Functor can be obtained (either independently, or
  // recursively by derivation)
  implicit def baseFunctorA[F0[_]]
    (implicit ff: Lazy[Functor[F0]]): FunctorA[F0[Arbitrary]] { type F[t] = F0[t] } =
      new FunctorA[F0[Arbitrary]] {
        type F[t] = F0[t]
        def map[A, B](fa: F[A])(f: A => B): F[B] = ff.value.map(fa)(f)
      }
}

trait FunctorA1 extends FunctorA2 {
  // FunctorA for Id
  implicit val idFunctorA: FunctorA[Arbitrary] { type F[t] = t } =
    new FunctorA[Arbitrary] {
      type F[t] = t
      def map[A, B](fa: A)(f: A => B): B = f(fa)
    }
}

trait FunctorA2 {
  type Aux[FA, F0[_]] = FunctorA[FA] { type F[t] = F0[t] }

  def apply[FA](implicit f: FunctorA[FA]): Aux[FA, f.F] = f

  trait Arbitrary

  // FunctorA for Const[T]#λ
  implicit def constFunctorA[T]: FunctorA[T] { type F[t] = T } =
    new FunctorA[T] {
      type F[t] = T
      def map[A, B](fa: T)(f: A => B): T = fa
    }
}
