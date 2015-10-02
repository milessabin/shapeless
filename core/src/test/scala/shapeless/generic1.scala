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

  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object IList {
    def fromSeq[T](ts: Seq[T]): IList[T] =
      ts.foldRight(INil[T](): IList[T])(ICons(_, _))
  }

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

  /** This version of Pointed isn't complete & NOT working but it allows to show bugs in IsHCons1/ISCCons/Generic1 macro generation */
  trait Pointed[F[_]] { def point[A](a: A): F[A] }

  object Pointed extends Pointed0 {
    def apply[F[_]](implicit f: Lazy[Pointed[F]]): Pointed[F] = f.value

    implicit val idPointed: Pointed[Id] =
      new Pointed[Id] {
        def point[A](a: A): Id[A] = a
      }

    // Pointed can be built for Singleton types
    implicit def constSingletonPointed[T](implicit w: Witness.Aux[T]): Pointed[Const[T]#λ] =
      new Pointed[Const[T]#λ] {
        def point[A](a: A): T = w.value
      }

    implicit def isCPointedSingleSingleton[C](
      implicit w: Witness.Aux[C], pf: Lazy[Pointed[Const[C]#λ]]
    ): Pointed[({type λ[A] = Const[C]#λ[A] :+: Const[CNil]#λ[A] })#λ] =
      new Pointed[({type λ[A] = Const[C]#λ[A] :+: Const[CNil]#λ[A] })#λ] {
        def point[A](a: A): Const[C]#λ[A] :+: Const[CNil]#λ[A] = Inl(pf.value.point(a))
      }

    implicit def isCPointedSingle[F[_]](
      implicit pf: Lazy[Pointed[F]]
    ): Pointed[({type λ[A] = F[A] :+: Const[CNil]#λ[A] })#λ] =
      new Pointed[({type λ[A] = F[A] :+: Const[CNil]#λ[A] })#λ] {
        def point[A](a: A): F[A] :+: Const[CNil]#λ[A] = Inl(pf.value.point(a))
      }

  }

  trait Pointed0 extends Pointed1 {

    implicit def hcons[F[_]](implicit ihc: IsHCons1[F, Pointed, Pointed]): Pointed[F] =
      new Pointed[F] {
        def point[A](a: A): F[A] = {
          ihc.pack(ihc.fh.point(a), ihc.ft.point(a))
        }
      }

    implicit def ccons[F[_]](implicit ihc: IsCCons1[F, Pointed, Pointed]): Pointed[F] =
      new Pointed[F] {
        def point[A](a: A): F[A] = {
          ihc.pack(Left(ihc.fh.point(a)))
        }
      }

    implicit def generic[F[_]](implicit gen: Generic1[F, Pointed]): Pointed[F] =
      new Pointed[F] {
        def point[A](a: A): F[A] = gen.from(gen.fr.point(a))
      }

  }

  trait Pointed1 {

    // HACKING the fact that CNil can't be pointed
    implicit def isCPointedSimpleType: Pointed[({type λ[A] = A :+: Const[CNil]#λ[A] })#λ] =
      new Pointed[({type λ[A] = A :+: Const[CNil]#λ[A] })#λ] {
        def point[A](a: A): A :+: Const[CNil]#λ[A] = Inl(a)
      }


    implicit val constHNilPointed: Pointed[Const[HNil]#λ] =
      new Pointed[Const[HNil]#λ] {
        def point[A](a: A): HNil = HNil
      }

  }

  // Pointed syntax
  object pointedSyntax {
    implicit def pointedOps[A](a: A): PointedOps[A] = new PointedOps(a)

    class PointedOps[A](a: A) {
      def point[F[_]](implicit F: Pointed[F]): F[A] = F.point(a)
    }
  }

  trait Trivial1[F[_]]

  object Trivial1 {
    implicit def trivially[F[_]]: Trivial1[F] = new Trivial1[F] {}
  }

  trait Trivial10[F[_], T]

  object Trivial10 {
    implicit def trivially[F[_], T]: Trivial10[F, T] = new Trivial10[F, T] {}
  }

  trait Trivial01[T, F[_]]

  object Trivial01 {
    implicit def trivially[T, F[_]]: Trivial01[T, F] = new Trivial01[T, F] {}
  }

  trait Trivial11[F[_], T[_]]

  object Trivial11 {
    implicit def trivially[F[_], T[_]]: Trivial11[F, T] = new Trivial11[F, T] {}
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
    Generic1[IList, TC1]
    //
    // type aliases required here: see https://issues.scala-lang.org/browse/SI-6895
    type LList[T] = List[List[T]]
    Generic1[LList, TC1]
    type LPair[T] = IList[(T, T)]
    Generic1[LPair, TC1]
    type PList[T] = (IList[T], IList[T])
    Generic1[PList, TC1]
    type PIdList[T] = (T, List[T])
    Generic1[PIdList, TC1]
    type Either1[T] = Either[T, Int]
    Generic1[Either1, TC1]
    type Either2[T] = Either[Int, T]
    Generic1[Either2, TC1]

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

  trait Singleton1[T[_]]
  object Singleton1 {
    implicit val hnilInstance: Singleton1[Const[HNil]#λ] = new Singleton1[Const[HNil]#λ] {}
  }

  @Test
  def testSingletons {
    type Unit1[t] = Unit
    type None1[t] = None.type

    implicitly[Generic1[Unit1, Singleton1]]
    implicitly[Generic1[None1, Singleton1]]
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

  @Test
  def testPointed: Unit = {
    import pointedSyntax._

    type R0[t] = None.type :: HNil
    IsHCons1[R0, Pointed, Pointed]

    Pointed[Option]
  }

  @Test
  def testPartiallyApplied {
    implicitly[Trivial10[List, Int]]
    type FI[f[_]] = Trivial10[f, Int]
    implicitly[FI[List]]
    val g0 = Generic1[Foo, FI]
    typed[Trivial10[g0.R, Int]](g0.mkFrr)

    implicitly[Trivial01[Int, List]]
    type IF[f[_]] = Trivial01[Int, f]
    implicitly[IF[List]]
    val g1 = Generic1[Foo, IF]
    typed[Trivial01[Int, g0.R]](g1.mkFrr)

    implicitly[Trivial11[Set, List]]
    type FL[f[_]] = Trivial11[f, List]
    implicitly[FL[Set]]
    val g2 = Generic1[Foo, FL]
    typed[Trivial11[g2.R, List]](g2.mkFrr)

    implicitly[Trivial11[List, Set]]
    type LF[f[_]] = Trivial11[List, f]
    implicitly[LF[Set]]
    val g3 = Generic1[Foo, LF]
    typed[Trivial11[List, g3.R]](g3.mkFrr)

    type HC[t] = t :: HNil
    val ih0 = IsHCons1[HC, FI, Trivial1]
    typed[Trivial10[ih0.H, Int]](ih0.mkFhh)
    typed[Trivial1[ih0.T]](ih0.mkFtt)

    val ih1 = IsHCons1[HC, Trivial1, FI]
    typed[Trivial1[ih1.H]](ih1.mkFhh)
    typed[Trivial10[ih1.T, Int]](ih1.mkFtt)

    type CC[t] = t :+: CNil
    val ic0 = IsCCons1[CC, FI, Trivial1]
    typed[Trivial10[ic0.H, Int]](ic0.mkFhh)
    typed[Trivial1[ic0.T]](ic0.mkFtt)

    val ic1 = IsCCons1[CC, Trivial1, FI]
    typed[Trivial1[ic1.H]](ic1.mkFhh)
    typed[Trivial10[ic1.T, Int]](ic1.mkFtt)

    type LO[t] = List[Option[t]]
    val s0 = Split1[LO, FI, Trivial1]
    typed[Trivial10[s0.O, Int]](s0.mkFoo)
    typed[Trivial1[s0.I]](s0.mkFii)

    val s1 = Split1[LO, Trivial1, FI]
    typed[Trivial1[s1.O]](s1.mkFoo)
    typed[Trivial10[s1.I, Int]](s1.mkFii)
  }
}

object SplitTestDefns {
  trait Dummy1[F[_]]
  object Dummy1 {
    implicit def mkDummy1[F[_]]: Dummy1[F] = new Dummy1[F] {}
  }
}

class SplitTests {
  import SplitTestDefns._

  @Test
  def testBasics {
    illTyped("""
    Split1[List, Dummy1, Dummy1]
    """)

    Split1[({ type λ[t] = List[List[t]] })#λ, Dummy1, Dummy1]

    Split1[({ type λ[t] = List[List[List[t]]] })#λ, Dummy1, Dummy1]

    type LList[T] = List[List[T]]
    Split1[LList, Dummy1, Dummy1]

    type ListDiag[T] = List[(T, T)]
    Split1[ListDiag, Dummy1, Dummy1]

    type ListDiagL[T] = List[(T, List[T])]
    Split1[ListDiagL, Dummy1, Dummy1]

    illTyped("""
    Split1[({ type λ[t] = Either[Int, t] })#λ, Dummy1, Dummy1]
    """)

    illTyped("""
    Split1[({ type λ[t] = Either[t, Int] })#λ, Dummy1, Dummy1]
    """)

    Split1[({ type λ[t] = Either[Int, List[t]] })#λ, Dummy1, Dummy1]

    Split1[({ type λ[t] = Either[List[t], Int] })#λ, Dummy1, Dummy1]

    type DiagList[T] = (List[T], List[T])
    Split1[DiagList, Dummy1, Dummy1]

    illTyped("""
    Split1[({ type λ[t] = (t, t) })#λ, Dummy1, Dummy1]
    """)

    type DiDiag[T] = ((T, T), (T, T))
    Split1[DiDiag, Dummy1, Dummy1]

    illTyped("""
    Split1[({ type λ[t] = Int => t }), Dummy1, Dummy1]
    """)

    illTyped("""
    Split1[({ type λ[t] = t => Int })#λ, Dummy1, Dummy1]
    """)

    Split1[({ type λ[t] = Int => List[t] })#λ, Dummy1, Dummy1]

    Split1[({ type λ[t] = List[t] => Int })#λ, Dummy1, Dummy1]

    type HNil1[t] = HNil
    type HCons1[t] = t :: HNil
    type CNil1[t] = CNil
    type CCons[t] = t :+: CNil

    illTyped("""
    Split1[HNil1, Dummy1, Dummy1]
    """)

    illTyped("""
    Split1[HCons1, Dummy1, Dummy1]
    """)

    illTyped("""
    Split1[CNil1, Dummy1, Dummy1]
    """)

    illTyped("""
    Split1[CCons1, Dummy1, Dummy1]
    """)
  }
}
