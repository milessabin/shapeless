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

import shapeless.test._
import shapeless.test.illTyped
import shapeless.testutil.assertTypedEquals

package SingletonTypeTestsDefns {
  class ValueTest(val x: Int) extends AnyVal
}

class SingletonTypesTests {
  import SingletonTypeTestsDefns._
  import syntax.singleton._

  val wTrue = Witness(true)
  type True = wTrue.T
  val wFalse = Witness(false)
  type False = wFalse.T

  val w0 = Witness(0)
  type _0 = w0.T
  val w1 = Witness(1)
  type _1 = w1.T
  val w2 = Witness(2)
  type _2 = w2.T
  val w3 = Witness(3)
  type _3 = w3.T

  val wFoo = Witness('foo)
  type Foo = wFoo.T
  val wBar = Witness('bar)
  type Bar = wBar.T

  @Test
  def testRefine {
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

    val sFooSym = 'foo.narrow
    val sBarSym = 'bar.narrow

    sameTyped(sFooSym)(sFooSym)
    sameTyped(sFooSym)('foo)

    illTyped("""
      sameTyped(sFooSym)(sBarSym)
      sameTyped(sFooSym)('bar)
    """)
  }

  trait Show[T] {
    def show: String
  }

  object Show {
    implicit val showTrue  = new Show[True] { def show = "true" }
    implicit val showFalse = new Show[False] { def show = "false" }

    implicit val showOne   = new Show[_1] { def show = "One" }
    implicit val showTwo   = new Show[_2] { def show = "Two" }
    implicit val showThree = new Show[_3] { def show = "Three" }

    implicit val showFoo   = new Show[Foo] { def show = "'foo" }
    implicit val showBar   = new Show[Bar] { def show = "'bar" }
  }

  def show[T](t: T)(implicit s: Show[T]) = s.show

  @Test
  def testRefinedTypeClass {
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

    illTyped("""
      show(0.narrow)
    """)

    val sFoo = show('foo.narrow)
    assertEquals("'foo", sFoo)

    val sBar = show('bar.narrow)
    assertEquals("'bar", sBar)
  }

  trait LiteralShow[T] {
    def show: String
  }

  object LiteralShow {
    implicit val showTrue  = new LiteralShow[Witness.`true`.T] { def show = "true" }
    implicit val showFalse = new LiteralShow[Witness.`false`.T] { def show = "false" }

    implicit val showOne   = new LiteralShow[Witness.`1`.T] { def show = "One" }
    implicit val showTwo   = new LiteralShow[Witness.`2`.T] { def show = "Two" }
    implicit val showThree = new LiteralShow[Witness.`3`.T] { def show = "Three" }

    implicit val showFoo   = new LiteralShow[Witness.`'foo`.T] { def show = "'foo" }
    implicit val showBar   = new LiteralShow[Witness.`'bar`.T] { def show = "'bar" }
  }

  def literalShow[T](t: T)(implicit s: LiteralShow[T]) = s.show

  @Test
  def testRefinedLiteralTypeClass {
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

    illTyped("""
      literalShow(0.narrow)
    """)

    val sFoo = literalShow('foo.narrow)
    assertEquals("'foo", sFoo)

    val sBar = literalShow('bar.narrow)
    assertEquals("'bar", sBar)
  }

  trait LiteralsShow[-T] {
    def show: String
  }

  object LiteralsShow {
    implicit val showTrueFalse        = new LiteralsShow[HList.`true, false`.T] { def show = "true, false" }
    implicit val showOneOrTwoOrThree  = new LiteralsShow[Coproduct.`1, 2, 3`.T] { def show = "One | Two | Three" }
    implicit val showFooBar           = new LiteralsShow[HList.`'foo, 'bar`.T] { def show = "'foo, 'bar" }
  }

  def literalsShow[T](t: T)(implicit s: LiteralsShow[T]) = s.show

  @Test
  def testRefinedLiteralsTypeClass {
    val sTrueFalse = literalsShow(true.narrow :: false.narrow :: HNil)
    assertEquals("true, false", sTrueFalse)

    val sOne = literalsShow(Inl(1.narrow))
    assertEquals("One | Two | Three", sOne)

    val sTwo = literalsShow(Inr(Inl(2.narrow)))
    assertEquals("One | Two | Three", sTwo)

    val sThree = literalsShow(Inr(Inr(Inl(3.narrow))))
    assertEquals("One | Two | Three", sThree)

    illTyped("""
      literalsShow(true :: false :: HNil)
    """)

    val sFooBar = literalsShow('foo.narrow :: 'bar.narrow :: HNil)
    assertEquals("'foo, 'bar", sFooBar)
  }

  @Test
  def testWitness {
    val wTrue = Witness(true)
    val wFalse = Witness(false)

    sameTyped(wTrue)(wTrue)

    illTyped("""
      sameTyped(wTrue)(wFalse)
    """)

    val w13 = Witness(13)
    val w23 = Witness(23)

    sameTyped(w13)(w13)

    illTyped("""
      sameTyped(w13)(w23)
    """)

    val wFoo = Witness("foo")
    val wBar = Witness("bar")

    sameTyped(wFoo)(wFoo)

    illTyped("""
      sameTyped(wFoo)(wBar)
    """)

    val wFooSym = Witness('foo)
    val wBarSym = Witness('bar)

    sameTyped(wFooSym)(wFooSym)

    illTyped("""
      sameTyped(wFooSym)(wBarSym)
    """)
  }

  def convert(w: Witness): Witness.Aux[w.T] = w

  def boundedConvert2[B](w: Witness.Lt[B]): Witness.Aux[w.T] = w

  def testSingletonWitness {
    trait Bound
    object Foo extends Bound
    val bar = "bar"
    val wFoo = Witness(Foo)
    val wBar = Witness(bar)

    typed[Foo.type](wFoo.value)
    typed[bar.type](wBar.value)

    val cFoo = convert(Foo)
    val cBar = convert(bar)

    sameTyped(cFoo)(Witness(Foo))
    sameTyped(cBar)(Witness(bar))

    val bcFoo = boundedConvert2[Bound](Foo)
    val bcBar = boundedConvert2[String](bar)

    sameTyped(bcFoo)(Witness(Foo))
    sameTyped(bcBar)(Witness(bar))
  }

  class PathDependentSingleton1 {
    val o: AnyRef = new Object {}
    val wO = Witness(o)
    type OT = wO.T
    implicitly[OT =:= o.type]

    val x0: OT = wO.value
    val x1: o.type = wO.value

    val x2 = wO.value
    typed[o.type](x2)
    typed[OT](x2)
  }

  object PathDependentSingleton2 {
    val o: AnyRef = new Object {}
    val wO = Witness(o)
    type OT = wO.T
    implicitly[OT =:= o.type]

    val x0: OT = wO.value
    val x1: o.type = wO.value

    val x2 = wO.value
    typed[o.type](x2)
    typed[OT](x2)
  }

  @Test
  def testWitnessConversion {
    val cTrue = convert(true)
    val cFalse = convert(false)

    sameTyped(cTrue)(Witness(true))
    sameTyped(cFalse)(Witness(false))

    illTyped("""
      sameTyped(cTrue)(Witness(false))
    """)
    illTyped("""
      sameTyped(cFalse)(Witness(true))
    """)

    val c13 = convert(13)
    val c23 = convert(23)

    sameTyped(c13)(Witness(13))
    sameTyped(c23)(Witness(23))

    illTyped("""
      sameTyped(c13)(Witness(23))
    """)
    illTyped("""
      sameTyped(c23)(Witness(13))
    """)

    val cFoo = convert("foo")
    val cBar = convert("bar")

    sameTyped(cFoo)(Witness("foo"))
    sameTyped(cBar)(Witness("bar"))

    illTyped("""
      sameTyped(cFoo)(Witness("bar"))
    """)
    illTyped("""
      sameTyped(cBar)(Witness("foo"))
    """)

    val cFooSym = convert('foo)
    val cBarSym = convert('bar)

    sameTyped(cFooSym)(Witness('foo))
    sameTyped(cBarSym)(Witness('bar))

    illTyped("""
      sameTyped(cFooSym)(Witness('bar))
    """)
    illTyped("""
      sameTyped(cBarSym)(Witness('foo))
    """)
  }

  def boundedConvert(w: Witness.Lt[Int]): Witness.Aux[w.T] = w

  @Test
  def testBoundedWitnessConversion {
    val c13 = boundedConvert(13)
    sameTyped(c13)(Witness(13))
    illTyped("""
      sameTyped(c13)(Witness(23))
    """)

    illTyped("""
      boundedConvert(true)
    """)

    illTyped("""
      boundedConvert("foo")
    """)

    illTyped("""
      boundedConvert('foo)
    """)
  }

  def showLiteral(t: Witness)(implicit s: Show[t.T]) = s.show

  @Test
  def testLiteralTypeClass {
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

    val sFooSym = showLiteral('foo)
    assertEquals("'foo", sFooSym)

    val sBarSym = showLiteral('bar)
    assertEquals("'bar", sBarSym)

    illTyped("""
      showLiteral(0)
    """)
  }

  trait ShowWitness[T] {
    def show: String
  }

  object ShowWitness {
    implicit def showWitness[T](implicit w: Witness.Aux[T]) =
      new ShowWitness[T] {
        def show = w.value.toString
      }
  }

  def showWitness(w: Witness)(implicit s: ShowWitness[w.T]) = s.show

  @Test
  def testWitnessTypeClass {
    val sTrue = showWitness(true)
    assertEquals("true", sTrue)

    val sFalse = showWitness(false)
    assertEquals("false", sFalse)

    val sOne = showWitness(1)
    assertEquals("1", sOne)

    val sTwo = showWitness(2)
    assertEquals("2", sTwo)

    val sThree = showWitness(3)
    assertEquals("3", sThree)

    val sFooSym = showWitness('foo)
    assertEquals("'foo", sFooSym)

    val sBarSym = showWitness('bar)
    assertEquals("'bar", sBarSym)
  }

  def showWitnessWith(w: WitnessWith[Show]) = w.instance.show

  @Test
  def testWitnessWith {
    val sTrue = showWitnessWith(true)
    assertEquals("true", sTrue)

    val sFalse = showWitnessWith(false)
    assertEquals("false", sFalse)

    val sOne = showWitnessWith(1)
    assertEquals("One", sOne)

    val sTwo = showWitnessWith(2)
    assertEquals("Two", sTwo)

    val sThree = showWitnessWith(3)
    assertEquals("Three", sThree)

    val sFooSym = showWitnessWith('foo)
    assertEquals("'foo", sFooSym)

    val sBarSym = showWitnessWith('bar)
    assertEquals("'bar", sBarSym)
  }

  trait Rel[T] {
    type Out
  }

  object Rel {
    implicit def relTrue:  Rel[True]  { type Out = Int  } = new Rel[True]  { type Out = Int }
    implicit def relFalse: Rel[False] { type Out = String } = new Rel[False] { type Out = String }
  }

  def check(w: WitnessWith[Rel])(v: w.instance.Out) = v

  @Test
  def testWitnessWithOut {
    val relTrue = check(true)(23)
    typed[Int](relTrue)

    val relFalse = check(false)("foo")
    typed[String](relFalse)

    illTyped("""
      check(true)("foo")
    """)

    illTyped("""
      check(false)(23)
    """)

    illTyped("""
      check(23)(23)
    """)
  }

  @Test
  def testValueClass {
    val x = new ValueTest(5)
    illTyped("""
    val wX = Witness(x)
    """)
  }

  @Test
  def primitiveWiden {
    {
      val w = Widen[Witness.`2`.T]
      illTyped(" w(3) ", "type mismatch;.*")
      val n = w(2)
      val n0: Int = n
      illTyped(" val n1: Witness.`2`.T = n ", "type mismatch;.*")

      assertTypedEquals[Int](2, n)
    }

    {
      val w = Widen[Witness.`true`.T]
      illTyped(" w(false) ", "type mismatch;.*")
      val b = w(true)
      val b0: Boolean = b
      illTyped(" val b1: Witness.`true`.T = b ", "type mismatch;.*")

      assertTypedEquals[Boolean](true, b)
    }

    {
      val w = Widen[Witness.`"ab"`.T]
      illTyped(""" w("s") """, "type mismatch;.*")
      val s = w("ab")
      val s0: String = s
      illTyped(""" val s1: Witness.`"ab"`.T = s """, "type mismatch;.*")

      assertTypedEquals[String]("ab", s)
    }
  }

  @Test
  def symbolWiden {
    // Masks shapeless.syntax.singleton.narrowSymbol.
    // Having it in scope makes the illTyped tests fail in an unexpected way.
    def narrowSymbol = ???

    val w = Widen[Witness.`'ab`.T]
    illTyped(" w('s.narrow) ", "type mismatch;.*")
    val s = w('ab.narrow)
    val s0: Symbol = s
    illTyped(" val s1: Witness.`'ab`.T = s ", "type mismatch;.*")

    assertTypedEquals[Symbol]('ab, s)
  }

  @Test
  def aliasWiden {
    type T = Witness.`2`.T
    val w = Widen[T]
    illTyped(" w(3) ", "type mismatch;.*")
    val n = w(2)
    val n0: Int = n
    illTyped(" val n1: Witness.`2`.T = n ", "type mismatch;.*")

    assertTypedEquals[Int](2, n)
  }


  trait B
  case object A extends B

  @Test
  def singletonWiden {
    illTyped(" Widen[A.type] ", "could not find implicit value for parameter widen:.*")
  }

}

package SingletonTypeTestsAux {
  class Wrapper {
    sealed trait Sealed
    object Sealed {
      case object A extends Sealed
    }

    implicitly[Witness.Aux[Sealed.A.type]]
  }
}
