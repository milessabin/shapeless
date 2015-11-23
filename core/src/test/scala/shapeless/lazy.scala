/*
 * Copyright (c) 2013-14 Miles Sabin
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

import scala.language.reflectiveCalls

import org.junit.Test
import org.junit.Assert._

import scala.collection.mutable.ListBuffer

import test._

class LazyStrictTests {

  @Test
  def testEffectOrder {
    val effects = ListBuffer[Int]()

    implicit def lazyInt: Lazy[Int] = Lazy[Int]{ effects += 3 ; 23 }

    implicit def strictInt: Strict[Int] = Strict[Int]{ effects += 6 ; 23 }

    def summonLazyInt(implicit li: Lazy[Int]): Int = {
      effects += 2
      val i = li.value
      effects += 4
      i
    }

    def summonStrictInt(implicit li: Strict[Int]): Int = {
      effects += 7
      val i = li.value
      effects += 8
      i
    }

    effects += 1
    val il = summonLazyInt
    effects += 5
    val is = summonStrictInt
    effects += 9

    assertEquals(23, il)
    assertEquals(23, is)
    assertEquals(1 to 9, effects.toList)
  }

  @Test
  def testDefConversion {
    val effects = ListBuffer[Int]()

    def effectfulLazyInt: Int = { effects += 3 ; 23 }

    def useEffectfulLazyInt(li: Lazy[Int]): Int = {
      effects += 2
      val i = li.value
      effects += 4
      i
    }

    def effectfulStrictInt: Int = { effects += 6 ; 23 }

    def useEffectfulStrictInt(li: Strict[Int]): Int = {
      effects += 7
      val i = li.value
      effects += 8
      i
    }

    effects += 1
    val il = useEffectfulLazyInt(effectfulLazyInt)
    effects += 5
    val is = useEffectfulStrictInt(effectfulStrictInt)
    effects += 9

    assertEquals(23, il)
    assertEquals(23, is)
    assertEquals(1 to 9, effects.toList)
  }

  @Test
  def testLazyConversion {
    val effects = ListBuffer[Int]()

    lazy val effectfulLazyInt: Int = { effects += 3 ; 23 }
    lazy val effectfulStrictInt: Int = { effects += 6 ; 23 }

    def useEffectfulLazyInt(li: Lazy[Int]): Int = {
      effects += 2
      val i = li.value
      effects += 4
      i
    }

    def useEffectfulStrictInt(li: Strict[Int]): Int = {
      effects += 7
      val i = li.value
      effects += 8
      i
    }

    effects += 1
    val il = useEffectfulLazyInt(effectfulLazyInt)
    effects += 5
    val is = useEffectfulStrictInt(effectfulStrictInt)
    effects += 9

    assertEquals(23, il)
    assertEquals(23, is)
    assertEquals(1 to 9, effects.toList)
  }

  @Test
  def testInlineConversion {
    val effects = ListBuffer[Int]()

    def useEffectfulLazyInt(li: Lazy[Int]): Int = {
      effects += 3
      val i = li.value
      effects += 4
      i
    }

    def useEffectfulStrictInt(si: Strict[Int]): Int = {
      effects += 7
      val i = si.value
      effects += 8
      i
    }

    effects += 1
    val il = useEffectfulLazyInt({ effects += 2 ; 23 })
    effects += 5
    val is = useEffectfulStrictInt({ effects += 6 ; 23 })
    effects += 9

    assertEquals(23, il)
    assertEquals(23, is)
    assertEquals(1 to 9, effects.toList)
  }

  sealed trait List[+T]
  case class Cons[T](hd: T, tl: List[T]) extends List[T]
  sealed trait Nil extends List[Nothing]
  case object Nil extends Nil

  trait Show[T] {
    def apply(t: T): String
  }

  def show[T](t: T)(implicit s: Show[T]) = s(t)

  trait CommonShows {
    implicit def showInt: Show[Int] = new Show[Int] {
      def apply(t: Int) = t.toString
    }

    implicit def showNil: Show[Nil] = new Show[Nil] {
      def apply(t: Nil) = "Nil"
    }
  }

  object LazyShows extends CommonShows {
    implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
      def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
    }

    implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
      def apply(t: List[T]) = t match {
        case n: Nil => show(n)
        case c: Cons[T] => show(c)(sc.value)
      }
    }
  }

  object LazyStrictMixShows extends CommonShows {
    implicit def showCons[T](implicit st: Strict[Show[T]], sl: Strict[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
      def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
    }

    implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
      def apply(t: List[T]) = t match {
        case n: Nil => show(n)
        case c: Cons[T] => show(c)(sc.value)
      }
    }
  }

  @Test
  def testRecursive {
    val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

    val lazyRepr = {
      import LazyShows._
      show(l)
    }

    val strictRepr = {
      import LazyStrictMixShows._
      show(l)
    }

    val expectedRepr = "Cons(1, Cons(2, Cons(3, Nil)))"

    assertEquals(expectedRepr, lazyRepr)
    assertEquals(expectedRepr, strictRepr)
  }

  trait Foo[T]
  object Foo {
    implicit def mkFoo[T]: Foo[T] = new Foo[T] {}
  }

  @Test
  def testMultiple {
    val foos = Lazy.values[Foo[Int] :: Foo[String] :: Foo[Boolean] :: HNil]
    implicit val x :: y :: z :: HNil = foos

    typed[Foo[Int]](x)
    typed[Foo[String]](y)
    typed[Foo[Boolean]](z)

    val x1 = implicitly[Foo[Int]]
    val y1 = implicitly[Foo[String]]
    val z1 = implicitly[Foo[Boolean]]

    assertTrue(x1 eq x)
    assertTrue(y1 eq y)
    assertTrue(z1 eq z)
  }

  trait Bar[A] { def foo(a: A): Unit }
  object Bar {
    implicit val intBar = new Bar[Int] { def foo(x: Int) = () }
  }

  @Test
  def testEta {
    implicitly[Lazy[Bar[Int]]].value.foo _
    implicitly[Strict[Bar[Int]]].value.foo _
  }

  trait Baz[T] {
    type U
  }

  object Baz {
    def lazyBaz[T, U](t: T)(implicit bt: Lazy[Aux[T, U]]): Aux[T, U] = bt.value
    def strictBaz[T, U](t: T)(implicit bt: Strict[Aux[T, U]]): Aux[T, U] = bt.value

    type Aux[T, U0] = Baz[T] { type U = U0 }

    implicit val bazIS: Aux[Int, String] = new Baz[Int] { type U = String }
    implicit val bazBD: Aux[Boolean, Double] = new Baz[Boolean] { type U = Double }
  }

  @Test
  def testAux {
    val lIS = Baz.lazyBaz(23)
    val sIS = Baz.strictBaz(23)
    typed[Baz.Aux[Int, String]](lIS)
    typed[Baz.Aux[Int, String]](sIS)

    val lBD = Baz.lazyBaz(true)
    val sBD = Baz.strictBaz(true)
    typed[Baz.Aux[Boolean, Double]](lBD)
    typed[Baz.Aux[Boolean, Double]](sBD)
  }

  @Test
  def testExtractors {
    implicitly[Lazy[Generic[Symbol]]]
    implicitly[Strict[Generic[Symbol]]]

    val x = {
      case class Leaf[A](value: A)
      implicitly[Lazy[Generic[Leaf[Int]]]]
      implicitly[Strict[Generic[Leaf[Int]]]]
      ()
    }
  }


  case class CC(l: List[CC])

  trait TC[T] {
    def repr(depth: Int): String
  }

  object TC {
    def apply[T](implicit tc: TC[T]): TC[T] = tc

    def instance[T](repr0: Int => String): TC[T] =
      new TC[T] {
        def repr(depth: Int) =
          if (depth < 0)
            "…"
          else
            repr0(depth)
      }
  }

  object TC0 extends TCImplicits[Lazy,   Strict, Strict]
  object TC1 extends TCImplicits[Strict, Lazy,   Strict]
  object TC2 extends TCImplicits[Strict, Strict, Lazy  ]
  object TC3 extends TCImplicits[Strict, Strict, Strict]

  trait TCImplicits[A[T] <: { def value: T }, B[T] <: { def value: T }, C[T] <: { def value: T }] {
    implicit def listTC[T](implicit underlying: A[TC[T]]): TC[List[T]] =
      TC.instance(depth => s"List(${underlying.value.repr(depth - 1)})")

    implicit def hnilTC: TC[HNil] =
      TC.instance(_ => "HNil")

    implicit def hconsTC[H, T <: HList]
     (implicit
       headTC: B[TC[H]],
       tailTC: TC[T]
     ): TC[H :: T] =
      TC.instance(depth => s"${headTC.value.repr(depth - 1)} :: ${tailTC.repr(depth)}")

    implicit def genericTC[F, G]
     (implicit
       gen: Generic.Aux[F, G],
       underlying: C[TC[G]]
     ): TC[F] =
      TC.instance(depth => s"Generic(${underlying.value.repr(depth - 1)})")
  }

  /** Illustrates that a single `Lazy` is enough to break a cycle */
  @Test
  def testCycle {
    val (ccTC0, genTC0, listTC0) = {
      import TC0._
      (TC[CC], TC[List[CC] :: HNil], TC[List[CC]])
    }

    val (ccTC1, genTC1, listTC1) = {
      import TC1._
      (TC[CC], TC[List[CC] :: HNil], TC[List[CC]])
    }

    val (ccTC2, genTC2, listTC2) = {
      import TC2._
      (TC[CC], TC[List[CC] :: HNil], TC[List[CC]])
    }

    val (ccTC3SO, genTC3SO, listTC3SO) = {
      import TC3._
      def throwsStackOverflow[T](f: => T): Boolean =
        try { f; false }
        catch { case _: StackOverflowError => true }

      (throwsStackOverflow(TC[CC]), throwsStackOverflow(TC[List[CC] :: HNil]), throwsStackOverflow(TC[List[CC]]))
    }

    val expectedCCRepr = "Generic(List(Generic(List(Generic(… :: HNil)) :: HNil)) :: HNil)"
    val expectedGenRepr = "List(Generic(List(Generic(List(…) :: HNil)) :: HNil)) :: HNil"
    val expectedListRepr = "List(Generic(List(Generic(List(Generic(…)) :: HNil)) :: HNil))"

    assert(ccTC0.repr(7) == expectedCCRepr)
    assert(genTC0.repr(7) == expectedGenRepr)
    assert(listTC0.repr(7) == expectedListRepr)

    assert(ccTC1.repr(7) == expectedCCRepr)
    assert(genTC1.repr(7) == expectedGenRepr)
    assert(listTC1.repr(7) == expectedListRepr)

    assert(ccTC2.repr(7) == expectedCCRepr)
    assert(genTC2.repr(7) == expectedGenRepr)
    assert(listTC2.repr(7) == expectedListRepr)

    assert(ccTC3SO)
    assert(genTC3SO)
    assert(listTC3SO)
  }
}
