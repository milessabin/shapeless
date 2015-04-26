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

package shapeless

import org.junit.Test
import org.junit.Assert._

import test._

package Generic1TestsAux {
  trait TC1[F[_]]
  object TC1 extends TC10 {
    implicit def tc1Id: TC1[Id] = new TC1[Id] {}
  }

  trait TC10 {
    implicit def tc1[F[_]]: TC1[F] = new TC1[F] {}
  }

  trait TC2[L[_]]
  object TC2 {
    implicit def tc2[L[_]]: TC2[L] = new TC2[L] {}
  }

  trait Box[T]

  case class Foo[T](t: T)
  case class Bar[T](t: Box[T])
  case class Baz[T](t: T, s: String)

  sealed trait Cp[+T]
  case class CpA[+T](t: T) extends Cp[T]
  case class CpB[+T](t: T) extends Cp[T]
  case object CpC extends Cp[Nothing]
  case class CpD[+T](t: T, n: Cp[T]) extends Cp[T]

  case class Prod[T](t: T, ts: List[T])

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  sealed trait Overlapping1[+T]
  sealed trait OA1[+T] extends Overlapping1[T]
  case class OAC1[+T](t: T) extends OA1[T]
  sealed trait OB1[+T] extends Overlapping1[T]
  case class OBC1[+T](t: T) extends OB1[T]
  case class OAB1[+T](t: T) extends OA1[T] with OB1[T]

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

  trait Functor0 {
    implicit def constFunctor[T]: Functor[Const[T]#λ] =
      new Functor[Const[T]#λ] {
        def map[A, B](t: T)(f: A => B): T = t
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
}

class Generic1Tests {
  import Generic1TestsAux._

  @Test
  def testGeneric1: Unit = {
    Generic1[Foo, TC1]
    Generic1[Bar, TC1]
    Generic1[Baz, TC1]
    Generic1[Cp, TC1]
    Generic1[Some, TC1]
    Generic1[Option, TC1]
    Generic1[List, TC1]

    val gen0 = Generic1[Prod, TC2]

    val prod = Prod(23, List(1, 2, 3))
    val r = gen0.to(prod)
    typed[Int :: List[Int] :: HNil](r)
    assertEquals((23 :: List(1, 2, 3) :: HNil), r)

    val fr = gen0.fr
    typed[TC2[gen0.R]](fr)
    typed[TC2[({ type λ[t] = t :: List[t] :: HNil })#λ]](fr)
  }

  @Test
  def testOverlappingCoproducts1 {
    val gen = Generic1[Overlapping1, TC1]
    val o: Overlapping1[Int] = OAB1(1)
    val o0 = gen.to(o)
    typed[OAB1[Int] :+: OAC1[Int] :+: OBC1[Int] :+: CNil](o0)

    val s1 = gen.from(o0)
    typed[Overlapping1[Int]](s1)
  }

  @Test
  def testIsHCons1: Unit = {
    type L[t] = Id[t] :: t :: String :: (t, t) :: List[Option[t]] :: Option[t] :: List[t] :: HNil

    val ihc = the[IsHCons1[L, TC1, TC2]]
    val l: L[Int] = 23 :: 13 :: "foo" :: (7, 13) :: List(Some(5)) :: Some(11) :: List(1, 2, 3) :: HNil

    val (hd, tl) = ihc.unpack(l)

    typed[Int](hd)
    assertEquals(23, hd)

    typed[Id[Int] :: String :: (Int, Int) :: List[Option[Int]] :: Option[Int] :: List[Int] :: HNil](tl)
    assertEquals(13 :: "foo" :: (7, 13) :: List(Some(5)) :: Some(11) :: List(1, 2, 3) :: HNil, tl)

    val cons = ihc.pack((hd, tl))
    typed[L[Int]](cons)
    assertEquals(l, cons)

    type T[t] = (t, t) :: Option[t] :: HNil
    val ihcT = implicitly[IsHCons1[T, TC1, TC2]]
  }

  @Test
  def testFunctor: Unit = {
    import functorSyntax._

    type R0[t] = t :: HNil
    type R1[t] = t :+: CNil

    IsHCons1[R0, Functor, Functor]
    IsCCons1[R1, Functor, Functor]

    Functor[Id]
    Functor[Const[Int]#λ]
    Functor[Const[HNil]#λ]
    Functor[Const[CNil]#λ]

    Functor[R0]
    Functor[R1]

    Functor[Some]
    Functor[Const[None.type]#λ]
    Functor[Option]
    Functor[List]

    type Twin[t] = (t, t)
    Functor[Twin]

    type SS[t] = Some[Some[t]]
    Functor[SS]

    type SO[t] = Some[Option[t]]
    Functor[SO]

    type OS[t] = Option[Some[t]]
    Functor[OS]

    type OO[t] = Option[Option[t]]
    Functor[OO]

    type OL[t] = Option[List[t]]
    Functor[OL]

    type OT[t] = Option[(t, t)]
    Functor[OT]

    def transform[F[_]: Functor, A, B](ft: F[A])(f: A => B): F[B] = ft.map(f)

    // Option has a Functor
    val o = transform(Option("foo"))(_.length)
    assertEquals(Some(3), o)

    // List has a Functor
    val l = transform(List("foo", "wibble", "quux"))(_.length)
    assertEquals(List(3, 6, 4), l)

    // Any case class has a Functor
    val prod = Prod("Three", List("French", "Hens"))

    val p0 = transform(prod)(_.length)
    val p1 = prod.map(_.length)           // they also have Functor syntax ...

    val expectedProd = Prod(5, List(6, 4))
    assertEquals(expectedProd, p0)
    assertEquals(expectedProd, p1)

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
    assertEquals(expectedTree, t0)
    assertEquals(expectedTree, t1)
  }
}
