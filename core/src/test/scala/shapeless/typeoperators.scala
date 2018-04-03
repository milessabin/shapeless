/*
 * Copyright (c) 2011-16 Miles Sabin
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

import scala.util.Try

import org.junit.Test
import org.junit.Assert._

import newtype._, tag._, test._, testutil._

class TypeOperatorTests {
  import TypeOperatorTests._

  trait ATag

  object ATag {
    implicit def taggedToString[T](value: T with Tagged[ATag]): String = message

    val message = "This object has ATag tag type"
  }

  @Test
  def testImplicitScopeForTaggedType: Unit = {
    val x = tag[ATag](1)
    val s: String = x
    assertEquals(ATag.message, s)
  }

  @Test
  def testNewtype: Unit = {
    type MyString = Newtype[String, MyStringOps]

    def MyString(s : String) : MyString = newtype(s)

    case class MyStringOps(s : String) {
      def mySize = s.size
    }
    implicit val mkOps = MyStringOps

    val ms = MyString("foo")

    illTyped("""
      val s : String = ms
    """)
    illTyped("""
      val ms2 : MyString = "foo"
    """)
    illTyped("""
      ms.size
    """)

    assertEquals(3, ms.mySize)

    val s2 = "bar"
    val ms2 = MyString(s2)

    assertTrue(ms2 eq (s2 : AnyRef))
  }

  trait Foo {
    type T
    val t: T
  }

  object Foo {
    implicit def mkFoo: Foo { type T = Int } = new Foo { type T = Int ; val t = 23 }
  }

  trait Foo2[U] {
    type T
    val t: T
  }

  object Foo2 {
    implicit def mkFoo2: Foo2[Char] { type T = Int } = new Foo2[Char] { type T = Int ; val t = 23 }
  }

  trait Bar[T] {
    type U
    val tu: Either[T, U]
  }

  object Bar {
    implicit def mkBar1: Bar[Boolean] { type U = Int } = new Bar[Boolean] { type U = Int ; val tu = Right(23) }
    implicit def mkBar2: Bar[String] { type U = Double } = new Bar[String] { type U = Double ; val tu = Right(13.0) }
  }

  case class Baz(i: Int, s: String)

  @Test
  def testTheValues: Unit = {
    val foo = the[Foo]
    typed[Foo](foo)
    typed[Int](foo.t)

    val bar1 = the[Bar[Boolean]]
    typed[Bar[Boolean]](bar1)
    typed[Either[Boolean, Int]](bar1.tu)

    val bar2 = the[Bar[String]]
    typed[Bar[String]](bar2)
    typed[Either[String, Double]](bar2.tu)
  }

  @Test
  def testTheTypes: Unit = {
    val t: the.Foo.T = 23
    typed[Int](t)

    val tu1: Either[Boolean, the.`Bar[Boolean]`.U] = Right(23)
    typed[Either[Boolean, Int]](tu1)

    val tu2: Either[String, the.`Bar[String]`.U] = Right(23)
    typed[Either[String, Double]](tu2)
  }

  @Test
  def testTheErrors: Unit = {
    illTyped("the.`Ordering[Set[Int]]`.Ops", "No implicit Ordering defined for Set\\[Int].")
  }

  @Test
  def testTheQuantifiers: Unit = {
    def bar0[T, U0](implicit b: Bar[T] { type U = U0 }): Bar[T] { type U = U0 } = {
      val res = the[Bar[T]]
      res
    }

    // Note: Slightly different method signature in TypeOperator211Tests
    def bar1[T, U0](implicit b: Bar[T] { type U = U0 }): Option[U0] = {
      val res: Option[the.`Bar[T]`.U] = None
      res
    }

    val b0 = bar0[Boolean, Int]
    typed[Bar[Boolean] { type U = Int }](b0)

    val b1 = bar1[Boolean, Int]
    typed[Option[Int]](b1)
  }

  @Test
  def testTypeOf: Unit = {

    val t1: TypeOf.`Foo.mkFoo`.T = 23
    typed[Int](t1)

    val t2: TypeOf.`Foo.mkFoo: Foo`.T = 23
    typed[Int](t2)

    val tu1: Either[Boolean, TypeOf.`Bar.mkBar1: Bar[Boolean]`.U] = Right(23)
    typed[Either[Boolean, Int]](tu1)

    val tu2: Either[String, TypeOf.`the.apply: Bar[String]`.U] = Right(23)
    typed[Either[String, Double]](tu2)

    val tu3: Either[String, TypeOf.`the[Bar[String]]`.U] = Right(23)
    typed[Either[String, Double]](tu3)

    val indexedHList: TypeOf.`Generic[(String, Boolean)].to(("foo", true)).zipWithIndex`.type = {
      Generic[(String, Boolean)].to(("foo", true)).zipWithIndex
    }
    typed[(String, _0) :: (Boolean, Succ[_0]) :: HNil](indexedHList)

    implicit val genBaz: TypeOf.`Generic[Baz]`.type = cachedImplicit
    val reprBaz = genBaz.to(Baz(23, "foo"))
    typed[Int :: String :: HNil](reprBaz)
  }

  @Test
  def testRejectBogus: Unit = {
    try {
      the.Foo
      assert(false)
    } catch {
      case _: Throwable => // OK
    }

    //the.Unit  // illTyped fails for this expression

    implicit val u2: Unit = ()
    //the.Unit  // illTyped fails for this expression

    //the.Int   // illTyped fails for this expression

    implicit val i2: Int = 23
    //the.Int   // illTyped fails for this expression

    illTyped("""
    val blah = the.`package wibble`
    """)
  }

  @Test
  def testValueClass: Unit = {
    implicit val one: AValueClass = AValueClass(1L)

    val x = the[AValueClass]
    typed[AValueClass](x)
  }
}

object TypeOperatorTests {
  final case class AValueClass(l: Long) extends AnyVal
}
