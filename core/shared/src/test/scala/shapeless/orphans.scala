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

package OrphansTestDefns {
  trait Ca[T]

  object Ca extends Ca0 {
    val caFoo = new Ca[Foo] { override def toString = "Ca.caFoo" }
    implicit def caFoo0: Ca[Foo] = caFoo
  }

  trait Ca0 {
    trait Dummy
    val caFallback = new Ca[Dummy] { override def toString = "Ca.caFallback" }

    implicit def fallback[T]: Ca[T] = caFallback.asInstanceOf[Ca[T]]
  }

  object CaDeriver {
    trait Dummy2
    val caDerived = new Ca[Dummy2] { override def toString = "CaDeriver.caDerived" }

    implicit def derive[T: Generic]: Ca[T] = caDerived.asInstanceOf[Ca[T]]
  }

  object LowPriorityCaDeriver extends OrphanDeriver[Ca, CaDeriver.type]

  trait Cb[T]

  object Cb extends Cb0 {
    val cbFoo = new Cb[Foo] { override def toString = "Cb.cbFoo" }
    implicit def cbFoo0: Cb[Foo] = cbFoo
  }

  trait Cb0 {
    trait Dummy
    val cbFallback = new Cb[Dummy] { override def toString = "Cb.cbFallback" }

    implicit def fallback[T]: Cb[T] = cbFallback.asInstanceOf[Cb[T]]
  }

  object CbDeriver {
    trait Dummy2
    val cbDerived = new Cb[Dummy2] { override def toString = "CbDeriver.cbDerived" }

    implicit def derive[T: Generic]: Cb[T] = cbDerived.asInstanceOf[Cb[T]]
  }

  object LowPriorityCbDeriver extends OrphanDeriver[Cb, CbDeriver.type]

  case class Foo(i: Int)
  case class Bar(s: String)
  case class Baz(d: Double)
  object Baz {
    val caBaz = new Ca[Baz] { override def toString = "Baz.caBaz" }
    implicit def caBaz0: Ca[Baz] = caBaz

    val cbBaz = new Cb[Baz] { override def toString = "Baz.cbBaz" }
    implicit def cbBaz0: Cb[Baz] = cbBaz

    implicit val eqBaz: Eq[Baz] = new Eq[Baz] {
      override def toString = "Baz.eqBaz"
      def eqv(x: Baz, y: Baz) = x == y
    }
  }

  class Quux(b: Boolean)

  object MultiOrphans {
    implicit def caT[T](implicit orphan: Orphan[Ca, CaDeriver.type, T]): Ca[T] = orphan.instance
    implicit def cbT[T](implicit orphan: Orphan[Cb, CbDeriver.type, T]): Cb[T] = orphan.instance
  }

  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
    override def toString: String = toString(Set.empty)
    def toString(seen: Set[Eq[_]]): String = toString
  }

  trait Eq0 {
    implicit def fallback[T]: Eq[T] = new Eq[T] {
      override def toString = "Eq.fallback"
      def eqv(x: T, y: T): Boolean = x == y
    }
  }

  object Eq extends Eq0 {
    implicit def eqInt: Eq[Int] = new Eq[Int] {
      override def toString = "Eq.eqInt"
      def eqv(x: Int, y: Int) = x == y
    }

    implicit def eqString: Eq[String] = new Eq[String] {
      override def toString = "Eq.eqString"
      def eqv(x: String, y: String) = x == y
    }

    implicit def eqBoolean: Eq[Boolean] = new Eq[Boolean] {
      override def toString = "Eq.eqBoolean"
      def eqv(x: Boolean, y: Boolean) = x == y
    }

    implicit def eqFoo: Eq[Foo] = new Eq[Foo] {
      override def toString = "Eq.eqFoo"
      def eqv(x: Foo, y: Foo) = x == y
    }
  }

  object EqDeriver extends TypeClassCompanion[Eq] {
    object typeClass extends TypeClass[Eq] {
      def emptyProduct = new Eq[HNil] {
        override def toString = "emptyProduct"
        def eqv(x: HNil, y: HNil) = true
      }

      def product[H, T <: HList](h: Eq[H], t: Eq[T]) = new Eq[H :: T] {
        override def toString(seen: Set[Eq[_]]) =
          if(seen(this)) s"<loop>" else s"product(${h.toString(seen+this)}, ${t.toString(seen+this)})"

        def eqv(x: H :: T, y: H :: T): Boolean = h.eqv(x.head, y.head) && t.eqv(x.tail, y.tail)
      }

      def emptyCoproduct = new Eq[CNil] {
        override def toString = "emptyCoproduct"
        def eqv(x: CNil, y: CNil) = true
      }

      def coproduct[L, R <: Coproduct](l: => Eq[L], r: => Eq[R]) = new Eq[L :+: R] {
        override def toString(seen: Set[Eq[_]]) =
          if(seen(this)) s"<loop>" else s"coproduct(${l.toString(seen+this)}, ${r.toString(seen+this)})"

        def eqv(x: L :+: R, y: L :+: R): Boolean = (x, y) match {
          case (Inl(x), Inl(y)) => l.eqv(x, y)
          case (Inr(x), Inr(y)) => r.eqv(x, y)
          case _ => false
        }
      }

      def project[A, B](b: => Eq[B], ab: A => B, ba: B => A) = new Eq[A] {
        override def toString(seen: Set[Eq[_]]) =
          if(seen(this)) s"<loop>" else s"project(${b.toString(seen+this)})"

        def eqv(x: A, y: A): Boolean = b.eqv(ab(x), ab(y))
      }
    }
  }

  object LowPriorityEqDeriver extends OrphanDeriver[Eq, EqDeriver.type]

  object MultiEqOrphans {
    implicit def eqT[T](implicit orphan: Orphan[Eq, EqDeriver.type, T]): Eq[T] = orphan.instance
  }

  sealed trait Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](item: A) extends Tree[A]

  sealed trait SimpleRec
  case class SimpleCons(rec: SimpleRec) extends SimpleRec
  case object SimpleNil extends SimpleRec
}

class OrphansTest {
  import OrphansTestDefns._

  @Test
  def testNoOrphans: Unit = {
    // Implicit scope only
    assertEquals(Ca.caFoo, implicitly[Ca[Foo]])
    assertEquals(Ca.caFallback, implicitly[Ca[Bar]])
    assertEquals(Baz.caBaz, implicitly[Ca[Baz]])
    assertEquals(Ca.caFallback, implicitly[Ca[Quux]])
  }

  @Test
  def testStandardPriorityOrphans: Unit = {
    // Imported orphans dominate implicit scope
    import CaDeriver._

    assertEquals(CaDeriver.caDerived, implicitly[Ca[Foo]])
    assertEquals(CaDeriver.caDerived, implicitly[Ca[Bar]])
    assertEquals(CaDeriver.caDerived, implicitly[Ca[Baz]])
    assertEquals(Ca.caFallback, implicitly[Ca[Quux]])
  }

  @Test
  def testLowPriorityOrphans: Unit = {
    // Imported orphans dominate implicit scope if at least as precise
    import LowPriorityCaDeriver._

    assertEquals(Ca.caFoo, implicitly[Ca[Foo]])
    assertEquals(CaDeriver.caDerived, implicitly[Ca[Bar]])
    assertEquals(Baz.caBaz, implicitly[Ca[Baz]])
    assertEquals(Ca.caFallback, implicitly[Ca[Quux]])
  }

  @Test
  def testMultiOrphans: Unit = {
    // Mass derivation of instances of multiple type classes
    import MultiOrphans._

    assertEquals(Ca.caFoo, implicitly[Ca[Foo]])
    assertEquals(CaDeriver.caDerived, implicitly[Ca[Bar]])
    assertEquals(Baz.caBaz, implicitly[Ca[Baz]])
    assertEquals(Ca.caFallback, implicitly[Ca[Quux]])

    assertEquals(Cb.cbFoo, implicitly[Cb[Foo]])
    assertEquals(CbDeriver.cbDerived, implicitly[Cb[Bar]])
    assertEquals(Baz.cbBaz, implicitly[Cb[Baz]])
    assertEquals(Cb.cbFallback, implicitly[Cb[Quux]])
  }

  @Test
  def testDerivedNoOrphans: Unit = {
    assertEquals("Eq.eqInt", implicitly[Eq[Int]].toString)
    assertEquals("Eq.eqFoo", implicitly[Eq[Foo]].toString)
    assertEquals("Eq.fallback", implicitly[Eq[Bar]].toString)
    assertEquals("Baz.eqBaz", implicitly[Eq[Baz]].toString)
    assertEquals("Eq.fallback", implicitly[Eq[Quux]].toString)
    assertEquals("Eq.fallback", implicitly[Eq[SimpleRec]].toString)
    assertEquals("Eq.fallback", implicitly[Eq[Tree[Int]]].toString)
  }

  @Test
  def testDerivedStandardPriorityOrphans: Unit = {
    import EqDeriver._

    assertEquals("Eq.eqInt", implicitly[Eq[Int]].toString)
    assertEquals("project(product(Eq.eqInt, emptyProduct))", implicitly[Eq[Foo]].toString)
    assertEquals("project(product(Eq.eqString, emptyProduct))", implicitly[Eq[Bar]].toString)
    assertEquals("project(product(Eq.fallback, emptyProduct))", implicitly[Eq[Baz]].toString)
    assertEquals("Eq.fallback", implicitly[Eq[Quux]].toString)
    assertEquals("project(coproduct(project(product(project(<loop>), emptyProduct)), coproduct(project(emptyProduct), emptyCoproduct)))", implicitly[Eq[SimpleRec]].toString)
    assertEquals("project(coproduct(project(product(Eq.eqInt, emptyProduct)), coproduct(project(product(project(<loop>), product(project(<loop>), emptyProduct))), emptyCoproduct)))", implicitly[Eq[Tree[Int]]].toString)
  }

  @Test
  def testDerivedLowPriorityOrphans: Unit = {
    import LowPriorityEqDeriver._

    assertEquals("Eq.eqInt", implicitly[Eq[Int]].toString)
    assertEquals("Eq.eqFoo", implicitly[Eq[Foo]].toString)
    assertEquals("project(product(Eq.eqString, emptyProduct))", implicitly[Eq[Bar]].toString)
    assertEquals("Baz.eqBaz", implicitly[Eq[Baz]].toString)
    assertEquals("Eq.fallback", implicitly[Eq[Quux]].toString)
    assertEquals("project(coproduct(project(product(project(<loop>), emptyProduct)), coproduct(project(emptyProduct), emptyCoproduct)))", implicitly[Eq[SimpleRec]].toString)
    assertEquals("project(coproduct(project(product(Eq.eqInt, emptyProduct)), coproduct(project(product(project(<loop>), product(project(<loop>), emptyProduct))), emptyCoproduct)))", implicitly[Eq[Tree[Int]]].toString)
  }

  @Test
  def testMultiEqOrphans: Unit = {
    import MultiEqOrphans._

    assertEquals("Eq.eqInt", implicitly[Eq[Int]].toString)
    assertEquals("Eq.eqFoo", implicitly[Eq[Foo]].toString)
    assertEquals("project(product(Eq.eqString, emptyProduct))", implicitly[Eq[Bar]].toString)
    assertEquals("Baz.eqBaz", implicitly[Eq[Baz]].toString)
    assertEquals("Eq.fallback", implicitly[Eq[Quux]].toString)
    assertEquals("project(coproduct(project(product(project(<loop>), emptyProduct)), coproduct(project(emptyProduct), emptyCoproduct)))", implicitly[Eq[SimpleRec]].toString)
    assertEquals("project(coproduct(project(product(Eq.eqInt, emptyProduct)), coproduct(project(product(project(<loop>), product(project(<loop>), emptyProduct))), emptyCoproduct)))", implicitly[Eq[Tree[Int]]].toString)
  }
}
