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

import test.illTyped

trait CachedTC[T]
object CachedTC {
  implicit def mkTC[T] = new CachedTC[T] {}
  implicit val cached: CachedTC[String] = cachedImplicit
}

object CachedTest {
  implicit val i: CachedTC[Int] = cachedImplicit
}

class CachedTest {
  import CachedTest._

  @Test
  def testBasics {
    assertTrue(CachedTest.i != null)
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
  def testCompanion {
    assertTrue(CachedTC.cached != null)
    assertTrue(Bar.foo != null)
  }

  @Test
  def testDivergent {
    illTyped(
      "cachedImplicit[math.Ordering[Ordered[Int]]]"
    )
  }

  @Test
  def testNotFound1 {
    trait T[X]
    illTyped(
      "cachedImplicit[T[String]]"
    )
  }

  @Test
  def testNotFound2 {
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
  def testRefined {
    assert(Quux.gen != null)
    assert(Quux.gen eq Quux.gen0)

    val gen = Generic[Quux]
    assert(gen eq Quux.gen0)

    val q = Quux(23, "foo")
    val h: Int = gen.to(q).head
    val th: String = gen.to(q).tail.head
  }
}
