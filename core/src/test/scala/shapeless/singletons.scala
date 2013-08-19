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

import shapeless.test.illTyped

class SingletonTypesTests {
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

  def typed[T](t: => T) {}

  def sameTyped[T](t1: => T)(t2: => T) {}

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
  }

  def convert(w: Witness): Witness.Aux[w.T] = w

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
  }

  trait Rel[T] {
    type Out
  }

  object Rel {
    implicit def relTrue:  Rel[True]  { type Out = Int  } = new Rel[True]  { type Out = Int }
    implicit def relFalse: Rel[False] { type Out = String } = new Rel[False] { type Out = String }
  }

  def check(w: WitnessWith[Rel])(v: w.Out) = v

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
}
