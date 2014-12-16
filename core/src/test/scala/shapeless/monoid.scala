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

import org.junit.Test
import org.junit.Assert._

import test.illTyped

package MonoidAux {
  trait Monoid[T] {
    def zero: T
    def append(a: T, b: T): T
  }

  object Monoid {
    def apply[T](implicit mt: Lazy[Monoid[T]]): Monoid[T] = mt.value

    def mzero[T](implicit mt: Monoid[T]) = mt.zero

    implicit def booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      def zero = false
      def append(a: Boolean, b: Boolean) = a || b
    }

    implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
      def zero = 0
      def append(a: Int, b: Int) = a+b
    }

    implicit def doubleMonoid: Monoid[Double] = new Monoid[Double] {
      def zero = 0.0
      def append(a: Double, b: Double) = a+b
    }

    implicit def stringMonoid: Monoid[String] = new Monoid[String] {
      def zero = ""
      def append(a: String, b: String) = a+b
    }

    implicit def deriveHNil: Monoid[HNil] =
      new Monoid[HNil] {
        def zero = HNil
        def append(a: HNil, b: HNil) = HNil
      }

    implicit def deriveHCons[H, T <: HList]
      (implicit
        mh: Lazy[Monoid[H]],
        mt: Lazy[Monoid[T]]
      ): Monoid[H :: T] =
        new Monoid[H :: T] {
          def zero = mh.value.zero :: mt.value.zero
          def append(a: H :: T, b: H :: T) = mh.value.append(a.head, b.head) :: mt.value.append(a.tail, b.tail)
        }

    implicit def deriveInstance[F, G](implicit gen: Generic.Aux[F, G], mg: Lazy[Monoid[G]]): Monoid[F] =
      new Monoid[F] {
        def zero = gen.from(mg.value.zero)
        def append(a: F, b: F) = gen.from(mg.value.append(gen.to(a), gen.to(b)))
      }
  }

  trait MonoidSyntax[T] {
    def |+|(b: T): T
  }

  object MonoidSyntax {
    implicit def monoidSyntax[T](a: T)(implicit mt: Monoid[T]): MonoidSyntax[T] = new MonoidSyntax[T] {
      def |+|(b: T) = mt.append(a, b)
    }
  }
}

class MonoidTests {
  import MonoidAux._

  import MonoidSyntax._

  case class Foo(i: Int, s: String)
  case class Bar(b: Boolean, s: String, d: Double)
  case class Qux(u: java.util.UUID)

  @Test
  def testBasics {
    implicit val fooInstance/*: Monoid[Foo]*/ = Monoid[Foo]
    implicit val barInstance/*: Monoid[Bar]*/ = Monoid[Bar]

    val f = Foo(13, "foo") |+| Foo(23, "bar")
    assertEquals(Foo(36, "foobar"), f)

    val b = Bar(true, "foo", 1.0) |+| Bar(false, "bar", 3.0)
    assertEquals(Bar(true, "foobar", 4.0), b)
  }

  @Test
  def testAuto {
    val f = Foo(13, "foo") |+| Foo(23, "bar")
    assertEquals(Foo(36, "foobar"), f)

    val b = Bar(true, "foo", 1.0) |+| Bar(false, "bar", 3.0)
    assertEquals(Bar(true, "foobar", 4.0), b)
  }

  @Test
  def testNonMonoid {
    illTyped(
      """
        val quxInstance = Monoid[Qux]
      """)
  }
}
