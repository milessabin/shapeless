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

import labelled.FieldType
import test.illTyped

object CachedTestDefns {
  trait CachedTC[T]
  object CachedTC {
    implicit def mkTC[T] = new CachedTC[T] {}
    implicit val cached: CachedTC[String] = cachedImplicit
  }

  object CachedTest {
    implicit val i: CachedTC[Int] = cachedImplicit
    implicit lazy val il: CachedTC[Boolean] = cachedImplicit
  }

  // Cats/Algebra Eq
  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {
    implicit val eqInt: Eq[Int] =
      new Eq[Int] {
        def eqv(x: Int, y: Int): Boolean = x == y
      }

    implicit def eqGeneric[T, R]
      (implicit
        gen: Generic.Aux[T, R],
        eqRepr: Lazy[Eq[R]]
      ): Eq[T] =
        new Eq[T] {
          def eqv(x: T, y: T): Boolean =
            eqRepr.value.eqv(gen.to(x), gen.to(y))
        }

    // Base case for products
    implicit val eqHNil: Eq[HNil] = new Eq[HNil] {
      def eqv(x: HNil, y: HNil): Boolean = true
    }

    // Induction step for products
    implicit def eqHCons[H, T <: HList]
      (implicit
        eqH: Lazy[Eq[H]],
        eqT: Lazy[Eq[T]]
      ): Eq[H :: T] =
        new Eq[H :: T] {
          def eqv(x: H :: T, y: H :: T): Boolean =
            eqH.value.eqv(x.head, y.head) && eqT.value.eqv(x.tail, y.tail)
        }
  }

  implicit class EqOps[T](x: T)(implicit eqT: Eq[T]) {
    def ===(y: T): Boolean = eqT.eqv(x, y)
  }

  case class Wibble(i: Int)
  object Wibble {
    implicit val eqw: Eq[Wibble] = cachedImplicit
  }
}

class CachedTest {
  import CachedTestDefns._
  import CachedTest._

  @Test
  def testBasics: Unit = {
    assertTrue(CachedTest.i != null)
  }

  @Test
  def testLazy: Unit = {
    assertTrue(CachedTest.il != null)
  }

  trait Foo[A]
  object Foo {
    implicit def materialize[A]: Foo[A] = new Foo[A] {}
  }

  case class Bar(x: Int)
  object Bar {
    implicit val foo: Foo[Bar] = cachedImplicit
  }

  @Test
  def testCompanion: Unit = {
    assertTrue(CachedTC.cached != null)
    assertTrue(Bar.foo != null)
  }

  @Test
  def testDivergent: Unit = {
    illTyped(
      "cachedImplicit[math.Ordering[Ordered[Int]]]"
    )
  }

  @Test
  def testNotFound1: Unit = {
    trait T[X]
    illTyped(
      "cachedImplicit[T[String]]"
    )
  }

  @Test
  def testNotFound2: Unit = {
    @scala.annotation.implicitNotFound("No U[${X}]")
    trait U[X]
    illTyped(
      "cachedImplicit[U[String]]", "No U\\[String]"
    )
  }

  case class Quux(i: Int, s: String)
  object Quux {
    val gen0 = cachedImplicit[Generic[Quux]]
    implicit val gen: Generic.Aux[Quux, gen0.Repr] = gen0
  }

  @Test
  def testRefined: Unit = {
    assert(Quux.gen != null)
    assert(Quux.gen eq Quux.gen0)

    val gen = Generic[Quux]
    assert(gen eq Quux.gen0)

    val q = Quux(23, "foo")
    val h: Int = gen.to(q).head
    val th: String = gen.to(q).tail.head
  }

  case class Quux2(i: Int, s: String)
  object Quux2 {
    val gen0 = cachedImplicit[Generic[Quux2]]
    implicit val gen: Generic.Aux[Quux2, gen0.Repr] = gen0

    val lgen0 = cachedImplicit[LabelledGeneric[Quux2]]
    implicit val lgen: LabelledGeneric.Aux[Quux2, lgen0.Repr] = lgen0
  }

  @Test
  def testRefined2: Unit = {
    assert(Quux2.gen != null)
    assert(Quux2.gen eq Quux2.gen0)
    assert(Quux2.lgen != null)
    assert(Quux2.lgen eq Quux2.lgen0)

    val gen = Generic[Quux2]
    assert(gen eq Quux2.gen0)

    val lgen = LabelledGeneric[Quux2]
    assert(lgen eq Quux2.lgen0)

    val q = Quux2(23, "foo")

    val h: Int = gen.to(q).head
    val th: String = gen.to(q).tail.head

    val lh: FieldType[Witness.`'i`.T, Int] = lgen.to(q).head
    val lth: FieldType[Witness.`'s`.T, String] = lgen.to(q).tail.head
  }

  @Test
  def testLazyRecursion: Unit = {
    assert(Wibble.eqw != null)
  }
}
