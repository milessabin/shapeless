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

package ProductTypeClassAux {
  sealed trait Image[T]

  case class Atom[T](id: String) extends Image[T]

  case class Product[H, T <: HList](h: Image[H], name: String, t: Image[T]) extends Image[H :: T]
  case object EmptyProduct extends Image[HNil]

  case class Project[F, G](instance: Image[G]) extends Image[F]

  object Image extends LabelledProductTypeClassCompanion[Image] {
    implicit def intImage: Image[Int] = Atom[Int]("int")
    implicit def stringImage: Image[String] = Atom[String]("string")

    implicit class Syntax[T](t: T)(implicit dummy: Image[T]) {
      def image = dummy
    }

    object typeClass extends LabelledProductTypeClass[Image] {
      def product[H, T <: HList](name: String, h: Image[H], t: Image[T]) = Product(h, name, t)

      def emptyProduct = EmptyProduct

      def project[F, G](instance: => Image[G], to: F => G, from: G => F) = Project[F, G](instance)
    }
  }
}

package TypeClassAux {
  sealed trait Image[T]

  case class Atom[T](id: String) extends Image[T]

  case class Sum[L, R <: Coproduct](l: Image[L], name: String, r: Image[R]) extends Image[L :+: R]
  case object EmptyCoproduct extends Image[CNil]

  case class Product[H, T <: HList](h: Image[H], name: String, t: Image[T]) extends Image[H :: T]
  case object EmptyProduct extends Image[HNil]

  case class Project[F, G](instance: Image[G]) extends Image[F]

  object Image extends LabelledTypeClassCompanion[Image] {
    implicit def intImage: Image[Int] = Atom[Int]("int")
    implicit def stringImage: Image[String] = Atom[String]("string")

    implicit class Syntax[T](t: T)(implicit dummy: Image[T]) {
      def image = dummy
    }

    object typeClass extends LabelledTypeClass[Image] {
      def product[H, T <: HList](name: String, h: Image[H], t: Image[T]) = Product(h, name, t)

      def emptyProduct = EmptyProduct

      def coproduct[L, R <: Coproduct](name: String, l: => Image[L], r: => Image[R]) = Sum(l, name, r)

      def emptyCoproduct = EmptyCoproduct

      def project[F, G](instance: => Image[G], to: F => G, from: G => F) = Project[F, G](instance)
    }
  }
}

class ProductTypeClassTests {
  import ProductTypeClassAux._
  import Image.Syntax

  case class Foo(i: Int, s: String)
  val fooResult = Project(Product(Atom("int"), "i", Product(Atom("string"), "s", EmptyProduct)))

  case class Bar()
  val barResult = Project(EmptyProduct)

  val tupleResult = Project(Product(Atom("int"), "_1", Product(Atom("string"), "_2", EmptyProduct)))
  val unitResult = Project(EmptyProduct)

  sealed trait Cases[A, B]
  case class CaseA[A, B](a: A) extends Cases[A, B]
  case class CaseB[A, B](b1: B, b2: B) extends Cases[A, B]

  illTyped("""Image[Cases[Int, String]]""")
  illTyped("""implicitly[Image[Cases[Int, String]]]""")

  @Test
  def testManualSingle: Unit = {
    assertEquals(fooResult, Image[Foo])
  }

  @Test
  def testManualEmpty: Unit = {
    assertEquals(barResult, Image[Bar])
  }

  @Test
  def testManualTuple: Unit = {
    assertEquals(tupleResult, Image[(Int, String)])
  }

  @Test
  def testManualUnit: Unit = {
    assertEquals(unitResult, Image[Unit])
  }

  @Test
  def testAutoSingle: Unit = {
    assertEquals(fooResult, implicitly[Image[Foo]])
    assertEquals(fooResult, Foo(23, "foo").image)
  }

  @Test
  def testAutoEmpty: Unit = {
    assertEquals(barResult, implicitly[Image[Bar]])
    assertEquals(barResult, Bar().image)
  }

  @Test
  def testAutoTuple: Unit = {
    assertEquals(tupleResult, implicitly[Image[(Int, String)]])
    assertEquals(tupleResult, (23, "foo").image)
  }

  @Test
  def testAutoUnit: Unit = {
    assertEquals(unitResult, implicitly[Image[Unit]])
    assertEquals(unitResult, ().image)
  }
}

class TypeClassTests {
  import TypeClassAux._
  import Image.Syntax

  case class Foo(i: Int, s: String)
  val fooResult = Project(Product(Atom("int"), "i", Product(Atom("string"), "s", EmptyProduct)))

  case class Bar()
  val barResult = Project(EmptyProduct)

  val tupleResult = Project(Product(Atom("int"), "_1", Product(Atom("string"), "_2", EmptyProduct)))
  val unitResult = Project(EmptyProduct)

  sealed trait Cases[A, B]
  case class CaseA[A, B](a: A) extends Cases[A, B]
  case class CaseB[A, B](b1: B, b2: B) extends Cases[A, B]

  val casesResult =
    Project(
      Sum(
        Project(Product(Atom("int"), "a", EmptyProduct)),
        "CaseA",
        Sum(
          Project(Product(Atom("string"), "b1", Product(Atom("string"), "b2", EmptyProduct))),
          "CaseB",
          EmptyCoproduct
        )
      )
    )

  @Test
  def testManualSingle: Unit = {
    assertEquals(fooResult, Image[Foo])
  }

  @Test
  def testManualEmpty: Unit = {
    assertEquals(barResult, Image[Bar])
  }

  @Test
  def testManualMulti: Unit = {
    assertEquals(casesResult, Image[Cases[Int, String]])
  }

  @Test
  def testManualTuple: Unit = {
    assertEquals(tupleResult, Image[(Int, String)])
  }

  @Test
  def testManualUnit: Unit = {
    assertEquals(unitResult, Image[Unit])
  }

  @Test
  def testAutoSingle: Unit = {
    assertEquals(fooResult, implicitly[Image[Foo]])
    assertEquals(fooResult, Foo(23, "foo").image)
  }

  @Test
  def testAutoEmpty: Unit = {
    assertEquals(barResult, implicitly[Image[Bar]])
    assertEquals(barResult, Bar().image)
  }

  @Test
  def testAutoMulti: Unit = {
    assertEquals(casesResult, Image[Cases[Int, String]])
    assertEquals(casesResult, (CaseA(23): Cases[Int, String]).image)
  }

  @Test
  def testAutoTuple: Unit = {
    assertEquals(tupleResult, implicitly[Image[(Int, String)]])
    assertEquals(tupleResult, (23, "foo").image)
  }

  @Test
  def testAutoUnit: Unit = {
    assertEquals(unitResult, implicitly[Image[Unit]])
    assertEquals(unitResult, ().image)
  }
}
