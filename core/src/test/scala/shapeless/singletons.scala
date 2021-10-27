/*
 * Copyright (c) 2013-16 Miles Sabin
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

import shapeless.test._
import shapeless.test.illTyped
import shapeless.testutil.assertTypedEquals

package SingletonTypeTestsDefns {
  class ValueTest(val x: Int) extends AnyVal
}

class SingletonTypesTests {
  import SingletonTypeTestsDefns._
  import syntax.singleton._

  type True = true
  type False = false

  type _0 = 0
  type _1 = 1
  type _2 = 2
  type _3 = 3

  type Foo = "foo"
  type Bar = "bar"

  @Test
  def testRefine: Unit = {
    val sTrue = true.narrow
    val sFalse = false.narrow

    sameTyped(sTrue)(sTrue)
    sameTyped(sTrue)(true)

    illTyped("""
      sameTyped(sTrue)(sFalse)
      sameTyped(sTrue)(false)
    """)

    val s13 = 13.narrow
    val s23 = 23.narrow

    sameTyped(s13)(s13)
    sameTyped(s13)(13)

    illTyped("""
      sameTyped(s13)(s23)
      sameTyped(s13)(23)
    """)

    val sFoo = "foo".narrow
    val sBar = "bar".narrow

    sameTyped(sFoo)(sFoo)
    sameTyped(sFoo)("foo")

    illTyped("""
      sameTyped(sFoo)(sBar)
      sameTyped(sFoo)("bar")
    """)
  }

  trait Show[T] {
    def show: String
  }

  object Show {
    implicit val showTrue: Show[True] = new Show[True] { def show = "true" }
    implicit val showFalse: Show[False] = new Show[False] { def show = "false" }

    implicit val showOne: Show[_1] = new Show[_1] { def show = "One" }
    implicit val showTwo: Show[_2] = new Show[_2] { def show = "Two" }
    implicit val showThree: Show[_3] = new Show[_3] { def show = "Three" }

    implicit val showFoo: Show[Foo] = new Show[Foo] { def show = "'foo" }
    implicit val showBar: Show[Bar] = new Show[Bar] { def show = "'bar" }
  }

  def show[T](t: T)(implicit s: Show[T]) = s.show

  @Test
  def testRefinedTypeClass: Unit = {
    val sTrue = show(true.narrow)
    assertEquals("true", sTrue)

    val sFalse = show(false.narrow)
    assertEquals("false", sFalse)

    val sOne = show(1.narrow)
    assertEquals("One", sOne)

    val sTwo = show(2.narrow)
    assertEquals("Two", sTwo)

    val sThree = show(3.narrow)
    assertEquals("Three", sThree)

    illTyped("show(0.narrow)")

    val sFoo = show("foo".narrow)
    assertEquals("'foo", sFoo)

    val sBar = show("bar".narrow)
    assertEquals("'bar", sBar)
  }

  trait LiteralShow[T] {
    def show: String
  }

  object LiteralShow {
    implicit val showTrue: LiteralShow[true] = new LiteralShow[true] { def show = "true" }
    implicit val showFalse: LiteralShow[false] = new LiteralShow[false] { def show = "false" }

    implicit val showOne: LiteralShow[1] = new LiteralShow[1] { def show = "One" }
    implicit val showTwo: LiteralShow[2] = new LiteralShow[2] { def show = "Two" }
    implicit val showThree: LiteralShow[3] = new LiteralShow[3] { def show = "Three" }

    implicit val showFoo: LiteralShow["foo"] = new LiteralShow["foo"] { def show = "'foo" }
    implicit val showBar: LiteralShow["bar"] = new LiteralShow["bar"] { def show = "'bar" }
  }

  def literalShow[T](t: T)(implicit s: LiteralShow[T]) = s.show

  @Test
  def testRefinedLiteralTypeClass: Unit = {
    val sTrue = literalShow(true.narrow)
    assertEquals("true", sTrue)

    val sFalse = literalShow(false.narrow)
    assertEquals("false", sFalse)

    val sOne = literalShow(1.narrow)
    assertEquals("One", sOne)

    val sTwo = literalShow(2.narrow)
    assertEquals("Two", sTwo)

    val sThree = literalShow(3.narrow)
    assertEquals("Three", sThree)

    illTyped("literalShow(0.narrow)")

    val sFoo = literalShow("foo".narrow)
    assertEquals("'foo", sFoo)

    val sBar = literalShow("bar".narrow)
    assertEquals("'bar", sBar)
  }

  trait LiteralsShow[-T] {
    def show: String
  }

  object LiteralsShow {
    implicit val showTrueFalse: LiteralsShow[true :: false :: HNil] = new LiteralsShow[true :: false :: HNil] { def show = "true, false" }
    implicit val showOneOrTwoOrThree: LiteralsShow[1 :+: 2 :+: 3 :+: CNil] = new LiteralsShow[1 :+: 2 :+: 3 :+: CNil] { def show = "One | Two | Three" }
    implicit val showFooBar: LiteralsShow["foo" :: "bar" :: HNil] = new LiteralsShow["foo" :: "bar" :: HNil] { def show = "'foo, 'bar" }
  }

  def literalsShow[T](t: T)(implicit s: LiteralsShow[T]) = s.show

  @Test
  def testRefinedLiteralsTypeClass: Unit = {
    val sTrueFalse = literalsShow(true.narrow :: false.narrow :: HNil)
    assertEquals("true, false", sTrueFalse)

    val sOne = literalsShow(Inl(1.narrow))
    assertEquals("One | Two | Three", sOne)

    val sTwo = literalsShow(Inr(Inl(2.narrow)))
    assertEquals("One | Two | Three", sTwo)

    val sThree = literalsShow(Inr(Inr(Inl(3.narrow))))
    assertEquals("One | Two | Three", sThree)

    illTyped("literalsShow(true :: false :: HNil)")

    val sFooBar = literalsShow("foo".narrow :: "bar".narrow :: HNil)
    assertEquals("'foo, 'bar", sFooBar)
  }

  def showLiteral[K <: Singleton](t: K)(implicit s: Show[K]) = s.show

  @Test
  def testLiteralTypeClass: Unit = {
    val sTrue = showLiteral(true)
    assertEquals("true", sTrue)

    val sFalse = showLiteral(false)
    assertEquals("false", sFalse)

    val sOne = showLiteral(1)
    assertEquals("One", sOne)

    val sTwo = showLiteral(2)
    assertEquals("Two", sTwo)

    val sThree = showLiteral(3)
    assertEquals("Three", sThree)

    val sFooSym = showLiteral("foo")
    assertEquals("'foo", sFooSym)

    val sBarSym = showLiteral("bar")
    assertEquals("'bar", sBarSym)

    illTyped("showLiteral(0)")
  }

  @Test
  def primitiveWiden: Unit = {
    {
      val w = Widen[2]
      illTyped("w(3)")
      val n = w(2)
      val n0: Int = n
      illTyped("val n1: 2 = n")

      assertTypedEquals[Int](2, n)
    }

    {
      val w = Widen[true]
      illTyped("w(false)")
      val b = w(true)
      val b0: Boolean = b
      illTyped("val b1: true = b")

      assertTypedEquals[Boolean](true, b)
    }

    {
      val w = Widen["ab"]
      illTyped("""w("s")""", "type mismatch;.*")
      val s = w("ab")
      val s0: String = s
      illTyped("""val s1: "ab" = s""", "type mismatch;.*")

      assertTypedEquals[String]("ab", s)
    }
  }

  @Test
  def symbolWiden: Unit = {
    // Masks shapeless.syntax.singleton.narrowSymbol.
    // Having it in scope makes the illTyped tests fail in an unexpected way.
    def narrowSymbol = ???

    val w = Widen["ab"]
    illTyped("""w("s".narrow)""", "type mismatch;.*")
    val s = w("ab".narrow)
    val s0: String = s
    illTyped("""val s1: "ab" = s""", "type mismatch;.*")

    assertTypedEquals[String]("ab", s)
  }

  @Test
  def aliasWiden: Unit = {
    type T = 2
    val w = Widen[T]
    illTyped("w(3)")
    val n = w(2)
    val n0: Int = n
    illTyped("val n1: 2 = n")

    assertTypedEquals[Int](2, n)
  }

  trait B
  case object A extends B

  @Test
  def singletonWiden: Unit = {
    illTyped("Widen[A.type]")
  }

  @Test
  def testWitnessTypeRefType: Unit = {
    trait B1 {
      type T <: B
      def getT(implicit w: ValueOf[T]): T = w.value
    }
    
    case class A1() extends B1 {
      type T = A.type
    }

    assertTypedEquals[A.type](A1().getT, A)
  }
}

package SingletonTypeTestsAux {
  class Wrapper {
    sealed trait Sealed
    object Sealed {
      case object A extends Sealed
    }

    implicitly[ValueOf[Sealed.A.type]]
  }
}

package UnrefineTest {
  import shapeless._
  import shapeless.labelled.{FieldType, KeyTag}
  import shapeless.ops.record._
  import shapeless.syntax.singleton._

  trait Foo[A] {
    type Out
    def to(a: A): Out
  }

  object Foo {
    type Aux[A, Out0] = Foo[A] { type Out = Out0 }
    def apply[A](implicit foo: Foo[A]): Aux[A, foo.Out] = foo

    implicit def from[A](
      implicit gen: LabelledGeneric[A]
    ): Aux[A, gen.Repr] = new Foo[A] {
      type Out = gen.Repr
      def to(a: A): Out = gen.to(a)
    }
  }

  class Bar[A, HL <: HList](gen: Foo.Aux[A, HL]) {
    def modify[K, V, U](k: K, f: V => U)(
      implicit modifier: Modifier[HL, K, V, U]
    ): Bar[A, modifier.Out] = new Bar[A, modifier.Out](new Foo[A] {
      type Out = modifier.Out
      def to(a: A): Out = modifier.apply(gen.to(a), f)
    })

    def keys(implicit keys: Keys[HL]): keys.Out = keys()
  }

  final case class FooBar(x: String, y: Int)

  object Test {
    new Bar(Foo.from(LabelledGeneric[FooBar])).modify("y".narrow, (_: Int) * 2)
    new Bar(Foo.from(LabelledGeneric[FooBar])).keys
    new Bar(Foo.from(LabelledGeneric[FooBar])).modify("y".narrow, (_: Int) * 2).keys
  }
}

object VarArgsWitnessTest {
  trait Base
  class Dep[B <: Base with Singleton]
  object instance extends Base
  val dep = new Dep[instance.type]

  def varargs[B <: Base with Singleton](el: Dep[B]*)(implicit w: ValueOf[B]): Unit = ()
  def poly[B <: Base with Singleton](el1: Dep[B])(implicit w: ValueOf[B]): Unit = ()
  def poly[B <: Base with Singleton](el1: Dep[B], el2: Dep[B])(implicit w: ValueOf[B]): Unit = ()

  varargs(dep)
  varargs(dep, dep)
  poly(dep)
  poly(dep, dep)
}

object RefinedWithSingletonTypeWitnessTest {
  import nat._

  implicitly[ValueOf[_0]]
  implicitly[ValueOf[_1]]
  implicitly[ValueOf[None.type]]
}
