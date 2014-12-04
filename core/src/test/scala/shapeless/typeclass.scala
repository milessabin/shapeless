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

import labelled.{ field, FieldType }
import ops.record.Values
import test.illTyped

package ProductTypeClassAux {
  sealed trait Image[T]

  case class Atom[T](id: String) extends Image[T]

  case class Product[H, T <: HList](h: Image[H], name: String, t: Image[T]) extends Image[H :: T]
  case object EmptyProduct extends Image[HNil]

  case class Project[F, G](instance: Image[G]) extends Image[F]

  object Image {
    def apply[T](implicit dit: Lazy[Image[T]]): Image[T] = dit.value

    implicit def intImage: Image[Int] = Atom[Int]("int")
    implicit def stringImage: Image[String] = Atom[String]("string")

    implicit class Syntax[T](t: T)(implicit dummy: Image[T]) {
      def image = dummy
    }

    trait Wrap[KL] {
      type L
      val unwrap: Image[L]
    }

    object Wrap {
      type Aux[KL, L0] = Wrap[KL] { type L = L0 }
      def apply[KL, L0](img: Image[L0]): Aux[KL, L0] =
        new Wrap[KL] {
          type L = L0
          val unwrap = img
        }
    }

    implicit def deriveHNil: Wrap.Aux[HNil, HNil] = Wrap[HNil, HNil](EmptyProduct)

    implicit def deriveHCons[HK <: Symbol, HV, TKV <: HList, TV <: HList]
      (implicit
        ih: Lazy[Image[HV]],
        label: Witness.Aux[HK],
        it: Lazy[Wrap[TKV] { type L <: HList }]
      ): Wrap.Aux[FieldType[HK, HV] :: TKV, HV :: it.value.L] =
        Wrap[FieldType[HK, HV] :: TKV, HV :: it.value.L](Product(ih.value, label.value.name, it.value.unwrap))

    implicit def deriveProduct[T, LKV]
      (implicit lgen: LabelledGeneric.Aux[T, LKV], iw: Lazy[Wrap[LKV]]): Image[T] =
        Project[T, iw.value.L](iw.value.unwrap)
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

  object Image {
    def apply[T](implicit dit: Lazy[Image[T]]): Image[T] = dit.value

    implicit def intImage: Image[Int] = Atom[Int]("int")
    implicit def stringImage: Image[String] = Atom[String]("string")

    implicit class Syntax[T](t: T)(implicit dummy: Image[T]) {
      def image = dummy
    }

    trait Wrap[KL] {
      type L
      val unwrap: Image[L]
    }

    object Wrap {
      type Aux[KL, L0] = Wrap[KL] { type L = L0 }
      def apply[KL, L0](img: Image[L0]): Aux[KL, L0] =
        new Wrap[KL] {
          type L = L0
          val unwrap = img
        }
    }

    implicit def deriveHNil: Wrap.Aux[HNil, HNil] = Wrap[HNil, HNil](EmptyProduct)

    implicit def deriveHCons[HK <: Symbol, HV, TKV <: HList, TV <: HList]
      (implicit
        ih: Lazy[Image[HV]],
        label: Witness.Aux[HK],
        it: Lazy[Wrap[TKV] { type L <: HList }]
      ): Wrap.Aux[FieldType[HK, HV] :: TKV, HV :: it.value.L] =
        Wrap[FieldType[HK, HV] :: TKV, HV :: it.value.L](Product(ih.value, label.value.name, it.value.unwrap))

    implicit def deriveCNil: Wrap.Aux[CNil, CNil] = Wrap[CNil, CNil](EmptyCoproduct)

    implicit def deriveCCons[HK <: Symbol, HV, TKV <: Coproduct, TV <: Coproduct]
      (implicit
        ih: Lazy[Image[HV]],
        label: Witness.Aux[HK],
        it: Lazy[Wrap[TKV] { type L <: Coproduct }]
      ): Wrap.Aux[FieldType[HK, HV] :+: TKV, HV :+: it.value.L] =
        Wrap[FieldType[HK, HV] :+: TKV, HV :+: it.value.L](Sum(ih.value, label.value.name, it.value.unwrap))

    implicit def deriveInstance[T, LKV]
      (implicit lgen: LabelledGeneric.Aux[T, LKV], iw: Lazy[Wrap[LKV]]): Image[T] =
        Project[T, iw.value.L](iw.value.unwrap)
  }
}

class ProductTypeClassTests {
  import ProductTypeClassAux._
  import Image.Syntax

  case class Foo(i: Int, s: String)
  val fooResult =
    Project(
      Product(Atom("int"), "i", Product(Atom("string"), "s", EmptyProduct))
    )

  case class Bar()
  val barResult =
    Project(EmptyProduct)

  val tupleResult =
    Project(
      Product(Atom("int"), "_1", Product(Atom("string"), "_2", EmptyProduct))
    )

  val unitResult =
    Project(EmptyProduct)

  sealed trait Cases[A, B]
  case class CaseA[A, B](a: A) extends Cases[A, B]
  case class CaseB[A, B](b1: B, b2: B) extends Cases[A, B]

  illTyped("""Image[Cases[Int, String]]""")
  illTyped("""implicitly[Image[Cases[Int, String]]]""")

  @Test
  def testManualSingle {
    assertEquals(fooResult, Image[Foo])
  }

  @Test
  def testManualEmpty {
    assertEquals(barResult, Image[Bar])
  }

  @Test
  def testManualTuple {
    assertEquals(tupleResult, Image[(Int, String)])
  }

  @Test
  def testManualUnit {
    assertEquals(unitResult, Image[Unit])
  }

  @Test
  def testAutoSingle {
    assertEquals(fooResult, implicitly[Image[Foo]])
    assertEquals(fooResult, Foo(23, "foo").image)
  }

  @Test
  def testAutoEmpty {
    assertEquals(barResult, implicitly[Image[Bar]])
    assertEquals(barResult, Bar().image)
  }

  @Test
  def testAutoTuple {
    assertEquals(tupleResult, implicitly[Image[(Int, String)]])
    assertEquals(tupleResult, (23, "foo").image)
  }

  @Test
  def testAutoUnit {
    assertEquals(unitResult, implicitly[Image[Unit]])
    assertEquals(unitResult, ().image)
  }
}

class TypeClassTests {
  import TypeClassAux._
  import Image.Syntax

  case class Foo(i: Int, s: String)
  val fooResult =
    Project(
      Sum(
        Project(Product(Atom("int"), "i", Product(Atom("string"), "s", EmptyProduct))), "Foo",
        EmptyCoproduct
      )
    )

  case class Bar()
  val barResult =
    Project(
      Sum(
        Project(EmptyProduct), "Bar",
        EmptyCoproduct
      )
    )

  val tupleResult =
    Project(
      Sum(
        Project(Product(Atom("int"), "_1", Product(Atom("string"), "_2", EmptyProduct))), "Tuple2",
        EmptyCoproduct
      )
    )

  val unitResult =
    Project(
      Sum(
        Project(EmptyProduct), "Unit",
        EmptyCoproduct
      )
    )

  sealed trait Cases[A, B]
  case class CaseA[A, B](a: A) extends Cases[A, B]
  case class CaseB[A, B](b1: B, b2: B) extends Cases[A, B]

  val casesResult =
    Project(
      Sum(
        Project(Product(Atom("int"), "a", EmptyProduct)), "CaseA",
        Sum(
          Project(Product(Atom("string"), "b1", Product(Atom("string"), "b2", EmptyProduct))), "CaseB",
          EmptyCoproduct
        )
      )
    )

  /*
  @Test
  def testManualSingle {
    assertEquals(fooResult, Image[Foo])
  }

  @Test
  def testManualEmpty {
    assertEquals(barResult, Image[Bar])
  }
  */

  @Test
  def testManualMulti {
    assertEquals(casesResult, Image[Cases[Int, String]])
  }

  /*
  @Test
  def testManualTuple {
    assertEquals(tupleResult, Image[(Int, String)])
  }

  @Test
  def testManualUnit {
    assertEquals(unitResult, Image[Unit])
  }

  @Test
  def testAutoSingle {
    assertEquals(fooResult, implicitly[Image[Foo]])
    assertEquals(fooResult, Foo(23, "foo").image)
  }

  @Test
  def testAutoEmpty {
    assertEquals(barResult, implicitly[Image[Bar]])
    assertEquals(barResult, Bar().image)
  }
  */

  @Test
  def testAutoMulti {
    assertEquals(casesResult, Image[Cases[Int, String]])
    assertEquals(casesResult, (CaseA(23): Cases[Int, String]).image)
  }

  /*
  @Test
  def testAutoTuple {
    assertEquals(tupleResult, implicitly[Image[(Int, String)]])
    assertEquals(tupleResult, (23, "foo").image)
  }

  @Test
  def testAutoUnit {
    assertEquals(unitResult, implicitly[Image[Unit]])
    assertEquals(unitResult, ().image)
  }
  */
}
