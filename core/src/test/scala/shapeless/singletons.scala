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
  import SingletonTypes._
  import Witness.{ WitnessEq, WitnessLt }

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
    val sTrue = refine(true)
    val sFalse = refine(false)

    sameTyped(sTrue)(sTrue)
    sameTyped(sTrue)(true)

    illTyped("""
      sameTyped(sTrue)(sFalse)
      sameTyped(sTrue)(false)
    """)

    val s13 = refine(13)
    val s23 = refine(23)

    sameTyped(s13)(s13)
    sameTyped(s13)(13)

    illTyped("""
      sameTyped(s13)(s23)
      sameTyped(s13)(23)
    """)

    val sFoo = refine("foo")
    val sBar = refine("bar")

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
    val sTrue = show(refine(true))
    assertEquals("true", sTrue)

    val sFalse = show(refine(false))
    assertEquals("false", sFalse)

    val sOne = show(refine(1))
    assertEquals("One", sOne)

    val sTwo = show(refine(2))
    assertEquals("Two", sTwo)

    val sThree = show(refine(3))
    assertEquals("Three", sThree)

    illTyped("""
      show(refine(0))
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

  def convert(w: Witness): WitnessEq[w.T] = w

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

  def boundedConvert(w: WitnessLt[Int]): WitnessEq[w.T] = w

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
    implicit def showWitness[T](implicit w: WitnessEq[T]) =
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
}
