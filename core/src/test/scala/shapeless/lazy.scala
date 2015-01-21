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

import scala.collection.mutable.ListBuffer

import test._

class LazyTests {

  @Test
  def testEffectOrder {
    val effects = ListBuffer[Int]()

    implicit def lazyInt: Lazy[Int] = Lazy[Int]{ effects += 3 ; 23 }

    def summonLazyInt(implicit li: Lazy[Int]): Int = {
      effects += 2
      val i = li.value
      effects += 4
      i
    }

    effects += 1
    val i = summonLazyInt
    effects += 5

    assertEquals(23, i)
    assertEquals(List(1, 2, 3, 4, 5), effects.toList)
  }

  @Test
  def testDefConversion {
    val effects = ListBuffer[Int]()

    def effectfulInt: Int = { effects += 3 ; 23 }

    def useEffectfulInt(li: Lazy[Int]): Int = {
      effects += 2
      val i = li.value
      effects += 4
      i
    }

    effects += 1
    val i = useEffectfulInt(effectfulInt)
    effects += 5

    assertEquals(23, i)
    assertEquals(List(1, 2, 3, 4, 5), effects.toList)
  }

  @Test
  def testLazyConversion {
    val effects = ListBuffer[Int]()

    lazy val effectfulInt: Int = { effects += 3 ; 23 }

    def useEffectfulInt(li: Lazy[Int]): Int = {
      effects += 2
      val i = li.value
      effects += 4
      i
    }

    effects += 1
    val i = useEffectfulInt(effectfulInt)
    effects += 5

    assertEquals(23, i)
    assertEquals(List(1, 2, 3, 4, 5), effects.toList)
  }

  @Test
  def testInlineConversion {
    val effects = ListBuffer[Int]()

    def useEffectfulInt(li: Lazy[Int]): Int = {
      effects += 3
      val i = li.value
      effects += 4
      i
    }

    effects += 1
    val i = useEffectfulInt({ effects += 2 ; 23 })
    effects += 5

    assertEquals(23, i)
    assertEquals(List(1, 2, 3, 4, 5), effects.toList)
  }

  sealed trait List[+T]
  case class Cons[T](hd: T, tl: List[T]) extends List[T]
  sealed trait Nil extends List[Nothing]
  case object Nil extends Nil

  trait Show[T] {
    def apply(t: T): String
  }

  def show[T](t: T)(implicit s: Show[T]) = s(t)

  implicit def showInt: Show[Int] = new Show[Int] {
    def apply(t: Int) = t.toString
  }

  implicit def showNil: Show[Nil] = new Show[Nil] {
    def apply(t: Nil) = "Nil"
  }

  implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
    def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
  }

  implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
    def apply(t: List[T]) = t match {
      case n: Nil => show(n)
      case c: Cons[T] => show(c)(sc.value)
    }
  }

  @Test
  def testRecursive {
    val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

    val sl = show(l)
    assertEquals("Cons(1, Cons(2, Cons(3, Nil)))", sl)
  }

  trait Foo[T]
  object Foo {
    implicit def mkFoo[T]: Foo[T] = new Foo[T] {}
  }

  @Test
  def testMultiple {
    val foos = Lazy.values[Foo[Int] :: Foo[String] :: Foo[Boolean] :: HNil]
    implicit val x :: y :: z :: HNil = foos

    typed[Foo[Int]](x)
    typed[Foo[String]](y)
    typed[Foo[Boolean]](z)

    val x1 = implicitly[Foo[Int]]
    val y1 = implicitly[Foo[String]]
    val z1 = implicitly[Foo[Boolean]]

    assertTrue(x1 eq x)
    assertTrue(y1 eq y)
    assertTrue(z1 eq z)
  }

  trait Bar[A] { def foo(a: A): Unit }
  object Bar {
    implicit val intBar = new Bar[Int] { def foo(x: Int) = () }
  }

  @Test
  def testEta {
    implicitly[Lazy[Bar[Int]]].value.foo _
  }

  trait Baz[T] {
    type U
  }

  object Baz {
    def apply[T, U](t: T)(implicit bt: Lazy[Aux[T, U]]): Aux[T, U] = bt.value

    type Aux[T, U0] = Baz[T] { type U = U0 }

    implicit val bazIS: Aux[Int, String] = new Baz[Int] { type U = String }
    implicit val bazBD: Aux[Boolean, Double] = new Baz[Boolean] { type U = Double }
  }

  @Test
  def testAux {
    val bIS = Baz(23)
    typed[Baz.Aux[Int, String]](bIS)

    val bBD = Baz(true)
    typed[Baz.Aux[Boolean, Double]](bBD)
  }
}
