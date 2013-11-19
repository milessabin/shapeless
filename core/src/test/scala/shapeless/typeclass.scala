/*
 * Copyright (c) 2013 Miles Sabin 
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

package TypeClassAux {
  sealed trait Dummy[T]
  case class Product[H, T <: HList](h: Dummy[H], name: String, t: Dummy[T]) extends Dummy[H :: T]
  case object EmptyProduct extends Dummy[HNil]
  case class Project[F, G](instance: Dummy[G]) extends Dummy[F]
  case class NamedField[F](instance: Dummy[F], name: String) extends Dummy[F]
  case class NamedCase[F](instance: Dummy[F], name: String) extends Dummy[F]
  case class Sum[L, R <: Coproduct](l: Dummy[L], name: String, r: Dummy[R]) extends Dummy[L :+: R]
  case class Sum1[L](l: Dummy[L], name: String) extends Dummy[L :+: CNil]
  case class Base[T](id: String) extends Dummy[T]

  object Dummy extends TypeClassCompanion[Dummy] {
    implicit def intDummy: Dummy[Int] = Base[Int]("int")
    implicit def stringDummy: Dummy[String] = Base[String]("string")

    implicit class Syntax[T](t: T)(implicit dummy: Dummy[T]) {
      def frobnicate = dummy
    }
  }

  object DummyInstance extends TypeClass[Dummy] {
    def emptyProduct = EmptyProduct
    def project[F, G](instance: => Dummy[G], to: F => G, from: G => F) = Project[F, G](instance)
    override def namedProduct[H, T <: HList](h: Dummy[H], name: String, t: Dummy[T]) = Product(h, name, t)
    override def namedField[F](instance: Dummy[F], name: String) = NamedField(instance, name)
    override def namedCase[F](instance: Dummy[F], name: String) = NamedCase(instance, name)
    override def namedCoproduct[L, R <: Coproduct](l: => Dummy[L], name: String, r: => Dummy[R]) = Sum(l, name, r)
    override def namedCoproduct1[L](l: => Dummy[L], name: String) = Sum1(l, name)

    def product[H, T <: HList](h: Dummy[H], t: Dummy[T]) = sys.error("unexpected call to product")
    def coproduct[L, R <: Coproduct](l: => Dummy[L], r: => Dummy[R]) = sys.error("unexpected call to coproduct")
    override def coproduct1[L](l: => Dummy[L]) = sys.error("unexpected call to coproduct1")
  }
}

class TypeClassTests {
  import TypeClassAux._
  import Dummy.Syntax

  case class Foo(i: Int, s: String)
  val fooResult = Project(NamedCase(Product(Base("int"), "i", Product(Base("string"), "s", EmptyProduct)), "Foo"))

  sealed trait Cases[A, B]
  case class CaseA[A, B](a: A) extends Cases[A, B]
  case class CaseB[A, B](b1: B, b2: B) extends Cases[A, B]

  val casesResult = Project(
    Sum(NamedField(Base("int"), "a"), "CaseA", Sum1(
      Product(Base("string"), "b1", Product(
        Base("string"), "b2", EmptyProduct
      )), "CaseB"
    ))
  )

  @Test
  def testManualSingle {
    implicit val tc: ProductTypeClass[Dummy] = DummyInstance
    assertEquals(fooResult, tc.derive[Foo])
    illTyped("""tc.derive[Cases[Int, String]]""")
  }

  @Test
  def testManualMulti {
    implicit val tc: TypeClass[Dummy] = DummyInstance
    assertEquals(casesResult, tc.derive[Cases[Int, String]])
  }

  /*
  @Test
  def testParentCheck {
    implicit val tc: TypeClass[Dummy] = DummyInstance
    illTyped("tc.derive[CaseA[Int, String]]",
      "Attempting to derive a type class instance for class `CaseA` with sealed superclass.*"
    )

    {
      import TypeClass.ignoreParent
      tc.derive[CaseA[Int, String]]
    }
  }
  */

  @Test
  def testAutoSingle {
    import Dummy.auto._
    implicit val tc: ProductTypeClass[Dummy] = DummyInstance
    assertEquals(fooResult, implicitly[Dummy[Foo]])
    illTyped("""implicitly[Dummy[Cases[Int, String]]]""")
    assertEquals(fooResult, (null: Foo).frobnicate)
  }

  @Test
  def testAutoMulti {
    import Dummy.auto._
    implicit val tc: TypeClass[Dummy] = DummyInstance
    assertEquals(casesResult, tc.derive[Cases[Int, String]])
    assertEquals(casesResult, (null: Cases[Int, String]).frobnicate)
  }
}

// vim: expandtab:ts=2:sw=2
