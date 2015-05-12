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

import test._
import testutil._

import ops.coproduct._

class CoproductTests {
  type ISB = Int :+: String :+: Boolean :+: CNil
  type III = Int :+: Int :+: Int :+: CNil

  case class Foo(msg: String)
  case class Bar(msg: String)
  type FBI = Foo :+: Bar :+: Int :+: CNil

  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit

  type APB = Apple :+: Pear :+: Banana :+: CNil

  object size extends Poly1 {
    implicit val caseInt     = at[Int](_ => 1)
    implicit val caseString  = at[String](_.length)
    implicit val caseBoolean = at[Boolean](_ => 1)
  }

  @Test
  def testInject {
    implicitly[Inject[Int :+: CNil, Int]]
    implicitly[Inject[Int :+: Int :+: CNil, Int]]
    implicitly[Inject[Int :+: Int :+: Int :+: CNil, Int]]
    implicitly[Inject[String :+: Int :+: CNil, Int]]
    implicitly[Inject[Int :+: String :+: CNil, Int]]

    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)

    illTyped("""
      val foo4 = Coproduct[ISB](1.0)
    """)
    illTyped("""
      val foo4 = Coproduct[ISB](CNil)
    """)
  }

  @Test
  def testMatch {
    def cpMatch(v: ISB) = v match {
      case Inl(x) =>
        typed[Int](x)
      case Inr(Inl(x)) =>
        typed[String](x)
      case Inr(Inr(Inl(x))) =>
        typed[Boolean](x)
      case Inr(Inr(Inr(_))) => ??? // This impossible case required for exhaustivity
    }

    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)

    cpMatch(foo1)
    cpMatch(foo2)
    cpMatch(foo3)
  }

  @Test
  def testSelect {
    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)

    val sel1a = foo1.select[Int]
    typed[Option[Int]](sel1a)
    assertEquals(Some(23), sel1a)

    val sel1b = foo1.select[String]
    typed[Option[String]](sel1b)
    assertEquals(None, sel1b)

    val sel1c = foo1.select[Boolean]
    typed[Option[Boolean]](sel1c)
    assertEquals(None, sel1c)

    illTyped("""
      foo1.select[Double]
    """)

    val sel2a = foo2.select[Int]
    typed[Option[Int]](sel2a)
    assertEquals(None, sel2a)

    val sel2b = foo2.select[String]
    typed[Option[String]](sel2b)
    assertEquals(Some("foo"), sel2b)

    val sel2c = foo2.select[Boolean]
    typed[Option[Boolean]](sel2c)
    assertEquals(None, sel2c)

    illTyped("""
      foo2.select[Double]
    """)

    val sel3a = foo3.select[Int]
    typed[Option[Int]](sel3a)
    assertEquals(None, sel3a)

    val sel3b = foo3.select[String]
    typed[Option[String]](sel3b)
    assertEquals(None, sel3b)

    val sel3c = foo3.select[Boolean]
    typed[Option[Boolean]](sel3c)
    assertEquals(Some(true), sel3c)

    illTyped("""
      foo3.select[Double]
    """)

    val fbi1 = Coproduct[FBI](Foo("hi"))
    val fbi2 = Coproduct[FBI](Bar("hi"))
    val fbi3 = Coproduct[FBI](23)

    val sel4a = fbi1.select[Foo]
    assertTypedEquals[Option[Foo]](Some(Foo("hi")), sel4a)

    val sel4b = fbi2.select[Bar]
    assertTypedEquals[Option[Bar]](Some(Bar("hi")), sel4b)

    val sel4c = fbi3.select[Int]
    assertTypedEquals[Option[Int]](Some(23), sel4c)
  }

  @Test
  def testFlatMap {
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val is = Coproduct[I :+: S :+: CNil](1)
    val isd = Coproduct[I :+: S :+: D :+: CNil](1)

    object coIdentity extends Poly1 {
      implicit def default[A] = at[A](a => Coproduct[A :+: CNil](a))
    }

    val r1 = in1.flatMap(coIdentity)
    assertTypedEquals[I :+: CNil](in1, r1)
    val r2 = is.flatMap(coIdentity)
    assertTypedEquals[I :+: S :+: CNil](is, r2)

    object coSquare extends Poly1 {
      implicit def default[A] = at[A](a => Coproduct[A :+: A :+: CNil](a))
    }

    val r3 = in1.flatMap(coSquare)
    assertTypedEquals[I :+: I :+: CNil](Coproduct[I :+:I :+: CNil](1), r3)

    val r4 = is.flatMap(coSquare)
    assertTypedEquals[I :+: I :+: S :+: S :+: CNil](
      Coproduct[I :+: I :+: S :+: S :+: CNil](1), r4)

    object complex extends Poly1 {
      implicit def caseInt    = at[Int](i => Coproduct[S :+: CNil](i.toString))
      implicit def caseString = at[String](s => Coproduct[C :+: D :+: CNil](s(0)))
      implicit def caseDouble = at[Double](d => Coproduct[I :+: S :+: CNil](d.toInt))
    }

    val r5 = isd.flatMap(complex)
    assertTypedEquals[S :+: C :+: D :+: I :+: S :+: CNil](
      Coproduct[S :+: C :+: D :+: I :+: S :+: CNil]("1"), r5)
  }

  @Test
  def testMap {
    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)

    val foo1b = foo1 map size
    typed[III](foo1b)
    assertEquals(Inl(1), foo1b)

    val foo2b = foo2 map size
    typed[III](foo2b)
    assertEquals(Inr(Inl(3)), foo2b)

    val foo3b = foo3 map size
    typed[III](foo3b)
    assertEquals(Inr(Inr(Inl(1))), foo3b)
  }

  @Test
  def testUnify {
    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)

    val foo1b = foo1 map size
    val foo2b = foo2 map size
    val foo3b = foo3 map size

    val foo1c = foo1b.unify
    typed[Int](foo1c)
    assertEquals(1, foo1c)

    val foo2c = foo2b.unify
    typed[Int](foo2c)
    assertEquals(3, foo2c)

    val foo3c = foo3b.unify
    typed[Int](foo3c)
    assertEquals(1, foo3c)

    val f1 = Coproduct[APB](Apple())
    val f2 = Coproduct[APB](Pear())
    val f3 = Coproduct[APB](Banana())

    val f1b = f1.unify
    typed[Fruit](f1b)

    val f2b = f2.unify
    typed[Fruit](f2b)

    val f3b = f3.unify
    typed[Fruit](f3b)

    // See https://github.com/milessabin/shapeless/issues/242
    case class Foo[T](c: T)
    val existentials1 = Coproduct[Foo[Double] :+: Foo[Float] :+: CNil](Foo(23F)).unify
    val existentials2 = Coproduct[Foo[Double] :+: Foo[Float] :+: Foo[Int] :+: CNil](Foo(23F)).unify

    typed[Foo[_ >: Float with Double <: AnyVal]](existentials1)
    typed[Foo[_ >: Int with Float with Double <: AnyVal]](existentials2)
  }

  @Test
  def testFold {
    import poly.identity

    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)

    val foo1b = foo1 fold size
    val foo2b = foo2 fold size
    val foo3b = foo3 fold size

    typed[Int](foo1b)
    assertEquals(1, foo1b)

    typed[Int](foo2b)
    assertEquals(3, foo2b)

    typed[Int](foo3b)
    assertEquals(1, foo3b)

    val f1 = Coproduct[APB](Apple())
    val f2 = Coproduct[APB](Pear())
    val f3 = Coproduct[APB](Banana())

    val f1b = f1 fold identity
    typed[Fruit](f1b)

    val f2b = f2 fold identity
    typed[Fruit](f2b)

    val f3b = f3 fold identity
    typed[Fruit](f3b)
  }

  @Test
  def testWithKeys {
    import syntax.singleton._
    import union._
    import ops.union._

    type U = Union.`'i -> Int, 's -> String, 'b -> Boolean`.T
    val cKeys = Keys[U].apply()

    val u1 = Coproduct[ISB](23).zipWithKeys(cKeys)
    val v1 = u1.get('i)
    typed[Option[Int]](v1)
    assertEquals(Some(23), v1)
    assertEquals(None, u1.get('s))

    val u2 = Coproduct[ISB]("foo").zipWithKeys(cKeys)
    val v2 = u2.get('s)
    typed[Option[String]](v2)
    assertEquals(Some("foo"), v2)
    assertEquals(None, u2.get('b))

    val u3 = Coproduct[ISB](true).zipWithKeys(cKeys)
    val v3 = u3.get('b)
    typed[Option[Boolean]](v3)
    assertEquals(Some(true), v3)
    assertEquals(None, u3.get('i))

    illTyped("v3.get('d)")

    // key/value lengths must match up
    illTyped("u1.zipWithKeys(uKeys.tail)")

    // Explicit type argument

    {
      val u1 = Coproduct[ISB](23).zipWithKeys[HList.`'i, 's, 'b`.T]
      val v1 = u1.get('i)
      typed[Option[Int]](v1)
      assertEquals(Some(23), v1)
      assertEquals(None, u1.get('s))
    }

    {
      val u2 = Coproduct[ISB]("foo").zipWithKeys[HList.`'i, 's, 'b`.T]
      val v2 = u2.get('s)
      typed[Option[String]](v2)
      assertEquals(Some("foo"), v2)
      assertEquals(None, u2.get('b))
    }

    {
      val u3 = Coproduct[ISB](true).zipWithKeys[HList.`'i, 's, 'b`.T]
      val v3 = u3.get('b)
      typed[Option[Boolean]](v3)
      assertEquals(Some(true), v3)
      assertEquals(None, u3.get('i))

      illTyped("v3.get('d)")
    }

    illTyped(" Coproduct[ISB](true).zipWithKeys[HList.`'i, 's, 'b, 'd`.T] ")

  }

  @Test
  def testPartialOrdering {
    val (one, two, abc, xyz) =
      (Coproduct[ISB](1), Coproduct[ISB](2), Coproduct[ISB]("abc"), Coproduct[ISB]("xyz"))

    def assertPOEquals(expected: Option[Int], l: ISB, r: ISB)(implicit po: PartialOrdering[ISB]) = {
      val actual =  po.tryCompare(l, r) map { i => Some(if (i < 0) -1 else if (i > 0) 1 else 0) }
      assertEquals(s"${l} ${r}", expected, actual getOrElse None)
    }

    assertPOEquals(Some(0),  one, one)
    assertPOEquals(Some(-1), one, two)
    assertPOEquals(Some(1),  two, one)

    assertPOEquals(Some(0),  abc, abc)
    assertPOEquals(Some(-1), abc, xyz)
    assertPOEquals(Some(1),  xyz, abc)

    assertPOEquals(None, one, abc)
    assertPOEquals(None, abc, one)
  }

  @Test
  def testLength {
    val r1 = Coproduct[Int :+: CNil](123).length
    assertTypedEquals[Nat._1](Nat._1, r1)

    val r2 = Coproduct[Int :+: String :+: CNil](123).length
    assertTypedEquals[Nat._2](Nat._2, r2)

    val r3 = Coproduct[Int :+: String :+: Double :+: CNil](123).length
    assertTypedEquals[Nat._3](Nat._3, r3)

    val r4 = Coproduct[Int :+: String :+: Double :+: Char :+: CNil](123).length
    assertTypedEquals[Nat._4](Nat._4, r4)
  }

  @Test
  def testExtendRight {
    type S = String; type I = Int; type D = Double; type C = Char
    type CoI    = I :+: CNil
    type CoIS   = I :+: S :+: CNil
    type CoISD  = I :+: S :+: D :+: CNil
    type CoISDC = I :+: S :+: D :+: C :+: CNil

    val r1 = Coproduct[CoI](1).extendRight[S]
    assertTypedEquals[CoIS](Coproduct[CoIS](1), r1)

    val r2 = Coproduct[CoIS](1).extendRight[D]
    assertTypedEquals[CoISD](Coproduct[CoISD](1), r2)

    val r3 = Coproduct[CoISD](1).extendRight[C]
    assertTypedEquals[CoISDC](Coproduct[CoISDC](1), r3)
  }

  @Test
  def testExtendLeft {
    type S = String; type I = Int; type D = Double; type C = Char
    type CoI    = I :+: CNil
    type CoSI   = S :+: I :+: CNil
    type CoDSI  = D :+: S :+: I :+: CNil
    type CoCDSI = C :+: D :+: S :+: I :+: CNil

    val r1 = Coproduct[CoI](1).extendLeft[S]
    assertTypedEquals[CoSI](Coproduct[CoSI](1), r1)

    val r2 = Coproduct[CoSI](1).extendLeft[D]
    assertTypedEquals[CoDSI](Coproduct[CoDSI](1), r2)

    val r3 = Coproduct[CoDSI](1).extendLeft[C]
    assertTypedEquals[CoCDSI](Coproduct[CoCDSI](1), r3)
  }

  @Test
  def testExtendLeftBy {
    type S = String; type I = Int; type D = Double; type C = Char
    type CoI    = I :+: CNil
    type CoSI   = S :+: I :+: CNil
    type CoDSI  = D :+: S :+: I :+: CNil
    type CoCDSI = C :+: D :+: S :+: I :+: CNil
    val coi = Coproduct[CoI](1)

    val r1 = coi.extendLeftBy[CNil]
    assertTypedEquals[CoI](coi, r1)

    val r2 = coi.extendLeftBy[S :+: CNil]
    assertTypedEquals[CoSI](Coproduct[CoSI](1), r2)

    val r3 = coi.extendLeftBy[D :+: S :+: CNil]
    assertTypedEquals[CoDSI](Coproduct[CoDSI](1), r3)

    val r4 = coi.extendLeftBy[C :+: D :+: S :+: CNil]
    assertTypedEquals[CoCDSI](Coproduct[CoCDSI](1), r4)
  }

  @Test
  def testExtendRightBy {
    type S = String; type I = Int; type D = Double; type C = Char
    type CoI    = I :+: CNil
    type CoIS   = I :+: S :+: CNil
    type CoISD  = I :+: S :+: D :+: CNil
    type CoISDC = I :+: S :+: D :+: C :+: CNil
    val coi = Coproduct[CoI](1)

    val r1 = coi.extendRightBy[CNil]
    assertTypedEquals[CoI](coi, r1)

    val r2 = coi.extendRightBy[S :+: CNil]
    assertTypedEquals[CoIS](Coproduct[CoIS](1), r2)

    val r3 = coi.extendRightBy[S :+: D :+: CNil]
    assertTypedEquals[CoISD](Coproduct[CoISD](1), r3)

    val r4 = coi.extendRightBy[S :+: D :+: C :+: CNil]
    assertTypedEquals[CoISDC](Coproduct[CoISDC](1), r4)
  }

  @Test
  def testRotateLeft {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val in2 = Coproduct[I :+: S :+: CNil](1)
    val in3 = Coproduct[I :+: S :+: D :+: CNil](1)
    val in4 = Coproduct[I :+: S :+: D :+: C :+: CNil](1)

    { // rotateLeft(0)
      val r1 = in1.rotateLeft(0)
      assertTypedSame[I :+: CNil](in1, r1)
      val r2 = in2.rotateLeft(0)
      assertTypedSame[I :+: S :+: CNil](in2, r2)
      val r3 = in3.rotateLeft(0)
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r3)
      val r4 = in4.rotateLeft(0)
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r4)
    }

    { // rotateLeft[_0]
      val r1 = in1.rotateLeft[_0]
      assertTypedSame[I :+: CNil](in1, r1)
      val r2 = in2.rotateLeft[_0]
      assertTypedSame[I :+: S :+: CNil](in2, r2)
      val r3 = in3.rotateLeft[_0]
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r3)
      val r4 = in4.rotateLeft[_0]
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r4)
    }

    { // rotateLeft(n % size == 0)
      val r1 = in1.rotateLeft(1)
      assertTypedSame[I :+: CNil](in1, r1)
      val r2 = in1.rotateLeft(2)
      assertTypedSame[I :+: CNil](in1, r2)
      val r3 = in2.rotateLeft(2)
      assertTypedSame[I :+: S :+: CNil](in2, r3)
      val r4 = in2.rotateLeft(4)
      assertTypedSame[I :+: S :+: CNil](in2, r4)
      val r5 = in3.rotateLeft(3)
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r5)
      val r6 = in3.rotateLeft(6)
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r6)
      val r7 = in4.rotateLeft(4)
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r7)
      val r8 = in4.rotateLeft(8)
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r8)
    }

    { // rotateLeft[N % Size == 0]
      val r1 = in1.rotateLeft[_1]
      assertTypedSame[I :+: CNil](in1, r1)
      val r2 = in1.rotateLeft[_2]
      assertTypedSame[I :+: CNil](in1, r2)
      val r3 = in2.rotateLeft[_2]
      assertTypedSame[I :+: S :+: CNil](in2, r3)
      val r4 = in2.rotateLeft[_4]
      assertTypedSame[I :+: S :+: CNil](in2, r4)
      val r5 = in3.rotateLeft[_3]
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r5)
      val r6 = in3.rotateLeft[_6]
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r6)
      val r7 = in4.rotateLeft[_4]
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r7)
      val r8 = in4.rotateLeft[_8]
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r8)
    }

    { // other(n)
      val r1 = in2.rotateLeft(1)
      assertTypedEquals[S :+: I :+: CNil](Coproduct[S :+: I :+: CNil](1), r1)

      val r2 = in3.rotateLeft(1)
      assertTypedEquals[S :+: D :+: I :+: CNil](Coproduct[S :+: D :+: I :+: CNil](1), r2)

      val r3 = in4.rotateLeft(1)
      assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), r3)

      val r4 = in4.rotateLeft(2)
      assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), r4)

      val r5 = in4.rotateLeft(3)
      assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), r5)

      val r6 = in4.rotateLeft(5)
      assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), r6)

      val r7 = in4.rotateLeft(6)
      assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), r7)
    }

    { // other[N]
      val r1 = in2.rotateLeft[_1]
      assertTypedEquals[S :+: I :+: CNil](Coproduct[S :+: I :+: CNil](1), r1)

      val r2 = in3.rotateLeft[_1]
      assertTypedEquals[S :+: D :+: I :+: CNil](Coproduct[S :+: D :+: I :+: CNil](1), r2)

      val r3 = in4.rotateLeft[_1]
      assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), r3)

      val r4 = in4.rotateLeft[_2]
      assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), r4)

      val r5 = in4.rotateLeft[_3]
      assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), r5)

      val r6 = in4.rotateLeft[_5]
      assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), r6)

      val r7 = in4.rotateLeft[_6]
      assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), r7)
    }

    {
      type C1 = Int :+: Boolean :+: String :+: Int :+: CNil
      type C2 = Boolean :+: String :+: Int :+: Int :+: CNil
      type C3 = String :+: Int :+: Int :+: Boolean :+: CNil
      type C4 = Int :+: Int :+: Boolean :+: String :+: CNil

      val i1: C1 = Inl(1)
      val i2: C2 = Inr(Inr(Inr(Inl(1))))
      val i3: C3 = Inr(Inr(Inl(1)))
      val i4: C4 = Inr(Inl(1))

      assertTypedEquals(i1, i1.rotateLeft(0))
      assertTypedEquals(i2, i1.rotateLeft(1))
      assertTypedEquals(i3, i1.rotateLeft(2))
      assertTypedEquals(i4, i1.rotateLeft(3))

      assertTypedEquals(i2, i2.rotateLeft(0))
      assertTypedEquals(i3, i2.rotateLeft(1))
      assertTypedEquals(i4, i2.rotateLeft(2))
      assertTypedEquals(i1, i2.rotateLeft(3))

      assertTypedEquals(i3, i3.rotateLeft(0))
      assertTypedEquals(i4, i3.rotateLeft(1))
      assertTypedEquals(i1, i3.rotateLeft(2))
      assertTypedEquals(i2, i3.rotateLeft(3))

      assertTypedEquals(i4, i4.rotateLeft(0))
      assertTypedEquals(i1, i4.rotateLeft(1))
      assertTypedEquals(i2, i4.rotateLeft(2))
      assertTypedEquals(i3, i4.rotateLeft(3))
    }
  }

  @Test
  def testRotateRight {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val in2 = Coproduct[I :+: S :+: CNil](1)
    val in3 = Coproduct[I :+: S :+: D :+: CNil](1)
    val in4 = Coproduct[I :+: S :+: D :+: C :+: CNil](1)

    { // rotateRight(0)
      val r1 = in1.rotateRight(0)
      assertTypedSame[I :+: CNil](in1, r1)
      val r2 = in2.rotateRight(0)
      assertTypedSame[I :+: S :+: CNil](in2, r2)
      val r3 = in3.rotateRight(0)
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r3)
      val r4 = in4.rotateRight(0)
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r4)
    }

    { // rotateRight[_0]
      val r1 = in1.rotateRight[_0]
      assertTypedSame[I :+: CNil](in1, r1)
      val r2 = in2.rotateRight[_0]
      assertTypedSame[I :+: S :+: CNil](in2, r2)
      val r3 = in3.rotateRight[_0]
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r3)
      val r4 = in4.rotateRight[_0]
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r4)
    }

    { // rotateRight(n % size == 0)
      val r1 = in1.rotateRight(1)
      assertTypedSame[I :+: CNil](in1, r1)
      val r2 = in1.rotateRight(2)
      assertTypedSame[I :+: CNil](in1, r2)
      val r3 = in2.rotateRight(2)
      assertTypedSame[I :+: S :+: CNil](in2, r3)
      val r4 = in2.rotateRight(4)
      assertTypedSame[I :+: S :+: CNil](in2, r4)
      val r5 = in3.rotateRight(3)
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r5)
      val r6 = in3.rotateRight(6)
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r6)
      val r7 = in4.rotateRight(4)
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r7)
      val r8 = in4.rotateRight(8)
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r8)
    }

    { // rotateRight[N % Size == 0]
      val r1 = in1.rotateRight[_1]
      assertTypedSame[I :+: CNil](in1, r1)
      val r2 = in1.rotateRight[_2]
      assertTypedSame[I :+: CNil](in1, r2)
      val r3 = in2.rotateRight[_2]
      assertTypedSame[I :+: S :+: CNil](in2, r3)
      val r4 = in2.rotateRight[_4]
      assertTypedSame[I :+: S :+: CNil](in2, r4)
      val r5 = in3.rotateRight[_3]
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r5)
      val r6 = in3.rotateRight[_6]
      assertTypedSame[I :+: S :+: D :+: CNil](in3, r6)
      val r7 = in4.rotateRight[_4]
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r7)
      val r8 = in4.rotateRight[_8]
      assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, r8)
    }

    { // other(n)
      val r1 = in2.rotateRight(1)
      assertTypedEquals[S :+: I :+: CNil](Coproduct[S :+: I :+: CNil](1), r1)

      val r2 = in3.rotateRight(1)
      assertTypedEquals[D :+: I :+: S :+: CNil](Coproduct[D :+: I :+: S :+: CNil](1), r2)

      val r3 = in4.rotateRight(1)
      assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), r3)

      val r4 = in4.rotateRight(2)
      assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), r4)

      val r5 = in4.rotateRight(3)
      assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), r5)

      val r6 = in4.rotateRight(5)
      assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), r6)

      val r7 = in4.rotateRight(6)
      assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), r7)
    }

    { // other[N]
      val r1 = in2.rotateRight[_1]
      assertTypedEquals[S :+: I :+: CNil](Coproduct[S :+: I :+: CNil](1), r1)

      val r2 = in3.rotateRight[_1]
      assertTypedEquals[D :+: I :+: S :+: CNil](Coproduct[D :+: I :+: S :+: CNil](1), r2)

      val r3 = in4.rotateRight[_1]
      assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), r3)

      val r4 = in4.rotateRight[_2]
      assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), r4)

      val r5 = in4.rotateRight[_3]
      assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), r5)

      val r6 = in4.rotateRight[_5]
      assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), r6)

      val r7 = in4.rotateRight[_6]
      assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), r7)
    }

    {
      type C1 = Int :+: Boolean :+: String :+: Int :+: CNil
      type C2 = Int :+: Int :+: Boolean :+: String :+: CNil
      type C3 = String :+: Int :+: Int :+: Boolean :+: CNil
      type C4 = Boolean :+: String :+: Int :+: Int :+: CNil

      val i1: C1 = Inl(1)
      val i2: C2 = Inr(Inl(1))
      val i3: C3 = Inr(Inr(Inl(1)))
      val i4: C4 = Inr(Inr(Inr(Inl(1))))

      assertTypedEquals(i1, i1.rotateRight(0))
      assertTypedEquals(i2, i1.rotateRight(1))
      assertTypedEquals(i3, i1.rotateRight(2))
      assertTypedEquals(i4, i1.rotateRight(3))

      assertTypedEquals(i2, i2.rotateRight(0))
      assertTypedEquals(i3, i2.rotateRight(1))
      assertTypedEquals(i4, i2.rotateRight(2))
      assertTypedEquals(i1, i2.rotateRight(3))

      assertTypedEquals(i3, i3.rotateRight(0))
      assertTypedEquals(i4, i3.rotateRight(1))
      assertTypedEquals(i1, i3.rotateRight(2))
      assertTypedEquals(i2, i3.rotateRight(3))

      assertTypedEquals(i4, i4.rotateRight(0))
      assertTypedEquals(i1, i4.rotateRight(1))
      assertTypedEquals(i2, i4.rotateRight(2))
      assertTypedEquals(i3, i4.rotateRight(3))
    }
  }

  @Test
  def testHead {
    val r1 = Coproduct[Int :+: CNil](1).head
    assertTypedEquals[Option[Int]](Some(1), r1)

    val r2 = Coproduct[Int :+: String :+: CNil](1).head
    assertTypedEquals[Option[Int]](Some(1), r2)

    val r3 = Coproduct[Int :+: String :+: CNil]("foo").head
    assertTypedEquals[Option[Int]](None, r3)
  }

  @Test
  def testTail {
    val r1 = Coproduct[Int :+: CNil](1).tail
    assertTypedEquals[Option[CNil]](None, r1)

    val r2 = Coproduct[Int :+: String :+: CNil](1).tail
    assertTypedEquals[Option[String :+: CNil]](None, r2)

    val r3 = Coproduct[Int :+: String :+: CNil]("foo").tail
    assertTypedEquals[Option[String :+: CNil]](Some(Coproduct[String :+: CNil]("foo")), r3)
  }

  @Test
  def testPrepend: Unit = {
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val in2 = Coproduct[I :+: S :+: CNil](1)
    val in3 = Coproduct[I :+: S :+: D :+: CNil](1)
    val in4 = Coproduct[I :+: S :+: D :+: C :+: CNil](1)

    {
      // Prepending CNil - checking for same-ness, not only equality
      val r1 = Prepend[CNil, I :+: CNil].apply(Right(in1))
      assertTypedSame(in1, r1)
      val r2 = Prepend[CNil, I :+: S :+: CNil].apply(Right(in2))
      assertTypedSame(in2, r2)
      val r3 = Prepend[CNil, I :+: S :+: D :+: CNil].apply(Right(in3))
      assertTypedSame(in3, r3)
      val r4 = Prepend[CNil, I :+: S :+: D :+: C :+: CNil].apply(Right(in4))
      assertTypedSame(in4, r4)
    }

    {
      // Appending CNil - checking for same-ness, not only equality
      val r1 = Prepend[I :+: CNil, CNil].apply(Left(in1))
      assertTypedSame(in1, r1)
      val r2 = Prepend[I :+: S :+: CNil, CNil].apply(Left(in2))
      assertTypedSame(in2, r2)
      val r3 = Prepend[I :+: S :+: D :+: CNil, CNil].apply(Left(in3))
      assertTypedSame(in3, r3)
      val r4 = Prepend[I :+: S :+: D :+: C :+: CNil, CNil].apply(Left(in4))
      assertTypedSame(in4, r4)
    }

    {
      val r11_1 = Prepend[I :+: CNil, I :+: CNil].apply(Left(in1))
      assertTypedEquals(Inl(1), r11_1)
      val r11_2 = Prepend[I :+: CNil, I :+: CNil].apply(Right(in1))
      assertTypedEquals(Inr(Inl(1)), r11_2)
      val r12_1 = Prepend[I :+: CNil, I :+: S :+: CNil].apply(Left(in1))
      assertTypedEquals(Inl(1), r12_1)
      val r12_2 = Prepend[I :+: CNil, I :+: S :+: CNil].apply(Right(in2))
      assertTypedEquals(Inr(Inl(1)), r12_2)

      val r34_3 = Prepend[I :+: S :+: D :+: CNil, I :+: S :+: D :+: C :+: CNil].apply(Left(in3))
      assertTypedEquals(Inl(1), r34_3)
      val r34_4 = Prepend[I :+: S :+: D :+: CNil, I :+: S :+: D :+: C :+: CNil].apply(Right(in4))
      assertTypedEquals(Inr(Inr(Inr(Inl(1)))), r34_4)
    }
  }

  @Test
  def testAlign {
    type K0 = Int :+: String :+: Boolean :+: CNil
    type K1 = Int :+: Boolean :+: String :+: CNil
    type K2 = String :+: Int :+: Boolean :+: CNil
    type K3 = String :+: Boolean :+: Int :+: CNil
    type K4 = Boolean :+: Int :+: String :+: CNil
    type K5 = Boolean :+: String :+: Int :+: CNil

    val k0i = Coproduct[K0](13)
    val k0s = Coproduct[K0]("bar")
    val k0b = Coproduct[K0](false)

    val k1i = Coproduct[K1](13)
    val k1s = Coproduct[K1]("bar")
    val k1b = Coproduct[K1](false)

    val k2i = Coproduct[K2](13)
    val k2s = Coproduct[K2]("bar")
    val k2b = Coproduct[K2](false)

    val k3i = Coproduct[K3](13)
    val k3s = Coproduct[K3]("bar")
    val k3b = Coproduct[K3](false)

    val k4i = Coproduct[K4](13)
    val k4s = Coproduct[K4]("bar")
    val k4b = Coproduct[K4](false)

    val k5i = Coproduct[K5](13)
    val k5s = Coproduct[K5]("bar")
    val k5b = Coproduct[K5](false)

    type C = K0

    val ci = Coproduct[C](23)
    val cs = Coproduct[C]("foo")
    val cb = Coproduct[C](true)

    val a0i = ci.align(k0i)
    assertTypedEquals[K0](Coproduct[K0](23), a0i)

    val a0s = cs.align(k0s)
    assertTypedEquals[K0](Coproduct[K0]("foo"), a0s)

    val a0b = cb.align(k0b)
    assertTypedEquals[K0](Coproduct[K0](true), a0b)

    val a1i = ci.align(k1i)
    assertTypedEquals[K1](Coproduct[K1](23), a1i)

    val a1s = cs.align(k1s)
    assertTypedEquals[K1](Coproduct[K1]("foo"), a1s)

    val a1b = cb.align(k1b)
    assertTypedEquals[K1](Coproduct[K1](true), a1b)

    val a2i = ci.align(k2i)
    assertTypedEquals[K2](Coproduct[K2](23), a2i)

    val a2s = cs.align(k2s)
    assertTypedEquals[K2](Coproduct[K2]("foo"), a2s)

    val a2b = cb.align(k2b)
    assertTypedEquals[K2](Coproduct[K2](true), a2b)

    val a3i = ci.align(k3i)
    assertTypedEquals[K3](Coproduct[K3](23), a3i)

    val a3s = cs.align(k3s)
    assertTypedEquals[K3](Coproduct[K3]("foo"), a3s)

    val a3b = cb.align(k3b)
    assertTypedEquals[K3](Coproduct[K3](true), a3b)

    val a4i = ci.align(k4i)
    assertTypedEquals[K4](Coproduct[K4](23), a4i)

    val a4s = cs.align(k4s)
    assertTypedEquals[K4](Coproduct[K4]("foo"), a4s)

    val a4b = cb.align(k4b)
    assertTypedEquals[K4](Coproduct[K4](true), a4b)

    val a5i = ci.align(k5i)
    assertTypedEquals[K5](Coproduct[K5](23), a5i)

    val a5s = cs.align(k5s)
    assertTypedEquals[K5](Coproduct[K5]("foo"), a5s)

    val a5b = cb.align(k5b)
    assertTypedEquals[K5](Coproduct[K5](true), a5b)

    val b0i = ci.align[K0]
    assertTypedEquals[K0](Coproduct[K0](23), b0i)

    val b0s = cs.align[K0]
    assertTypedEquals[K0](Coproduct[K0]("foo"), b0s)

    val b0b = cb.align[K0]
    assertTypedEquals[K0](Coproduct[K0](true), b0b)

    val b1i = ci.align[K1]
    assertTypedEquals[K1](Coproduct[K1](23), b1i)

    val b1s = cs.align[K1]
    assertTypedEquals[K1](Coproduct[K1]("foo"), b1s)

    val b1b = cb.align[K1]
    assertTypedEquals[K1](Coproduct[K1](true), b1b)

    val b2i = ci.align[K2]
    assertTypedEquals[K2](Coproduct[K2](23), b2i)

    val b2s = cs.align[K2]
    assertTypedEquals[K2](Coproduct[K2]("foo"), b2s)

    val b2b = cb.align[K2]
    assertTypedEquals[K2](Coproduct[K2](true), b2b)

    val b3i = ci.align[K3]
    assertTypedEquals[K3](Coproduct[K3](23), b3i)

    val b3s = cs.align[K3]
    assertTypedEquals[K3](Coproduct[K3]("foo"), b3s)

    val b3b = cb.align[K3]
    assertTypedEquals[K3](Coproduct[K3](true), b3b)

    val b4i = ci.align[K4]
    assertTypedEquals[K4](Coproduct[K4](23), b4i)

    val b4s = cs.align[K4]
    assertTypedEquals[K4](Coproduct[K4]("foo"), b4s)

    val b4b = cb.align[K4]
    assertTypedEquals[K4](Coproduct[K4](true), b4b)

    val b5i = ci.align[K5]
    assertTypedEquals[K5](Coproduct[K5](23), b5i)

    val b5s = cs.align[K5]
    assertTypedEquals[K5](Coproduct[K5]("foo"), b5s)

    val b5b = cb.align[K5]
    assertTypedEquals[K5](Coproduct[K5](true), b5b)

    illTyped("""
      (Coproduct[String :+: CNil]).align[Int :+: CNil]
    """)

    illTyped("""
      (Coproduct[String :+: Int :+: CNil]).align[String :+: CNil]
    """)

    illTyped("""
      (Coproduct[Int :+: CNil]).align[Int :+: String :+: CNil]
    """)
  }

  @Test
  def testReverse {
    {
      type S = String; type I = Int; type D = Double; type C = Char
      type SI = S :+: I :+: CNil; type IS = I :+: S :+: CNil

      val r1 = Coproduct[I :+: CNil](1).reverse
      assertTypedEquals[I :+: CNil](Coproduct[I :+: CNil](1), r1)

      val r2 = Coproduct[IS](1).reverse
      assertTypedEquals[SI](Coproduct[SI](1), r2)

      val r3 = Coproduct[IS]("foo").reverse
      assertTypedEquals[SI](Coproduct[SI]("foo"), r3)
    }

    {
      type C = Int :+: String :+: Double :+: Int :+: Boolean :+: CNil

      val c1: C = Inl(1)
      val c2: C = Inr(Inl("str"))
      val c3: C = Inr(Inr(Inl(3.0)))
      val c4: C = Inr(Inr(Inr(Inl(4))))
      val c5: C = Inr(Inr(Inr(Inr(Inl(true)))))

      assertTypedEquals(c1, c1.reverse.reverse)
      assertTypedEquals(c2, c2.reverse.reverse)
      assertTypedEquals(c3, c3.reverse.reverse)
      assertTypedEquals(c4, c4.reverse.reverse)
      assertTypedEquals(c5, c5.reverse.reverse)
    }
  }

  @Test
  def testInit {
    val r1 = Coproduct[Int :+: CNil](1).init
    assertTypedEquals[Option[CNil]](None, r1)

    val r2 = Coproduct[Int :+: String :+: CNil]("foo").init
    assertTypedEquals[Option[Int :+: CNil]](None, r2)

    val r3 = Coproduct[Int :+: String :+: CNil](1).init
    assertTypedEquals[Option[Int :+: CNil]](Some(Coproduct[Int :+: CNil](1)), r3)
  }

  @Test
  def testLast {
    val r1 = Coproduct[Int :+: CNil](1).last
    assertTypedEquals[Option[Int]](Some(1), r1)

    val r2 = Coproduct[Int :+: String :+: CNil]("foo").last
    assertTypedEquals[Option[String]](Some("foo"), r2)

    val r3 = Coproduct[Int :+: String :+: CNil](1).last
    assertTypedEquals[Option[String]](None, r3)
  }

  @Test
  def testAt {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val in2 = Coproduct[I :+: S :+: CNil](1)
    val in3 = Coproduct[I :+: S :+: D :+: CNil](1)

    {
      val r1 = in1.at(0)
      assertTypedEquals[Option[I]](Some(1), r1)

      val r2 = in2.at(0)
      assertTypedEquals[Option[I]](Some(1), r2)

      val r3 = in3.at(0)
      assertTypedEquals[Option[I]](Some(1), r3)


      val r4 = in2.at(1)
      assertTypedEquals[Option[S]](None, r4)

      val r5 = in3.at(1)
      assertTypedEquals[Option[S]](None, r5)

      val r6 = in3.at(2)
      assertTypedEquals[Option[D]](None, r6)
    }

    {
      val r1 = in1.at[nat._0]
      assertTypedEquals[Option[I]](Some(1), r1)

      val r2 = in2.at[nat._0]
      assertTypedEquals[Option[I]](Some(1), r2)

      val r3 = in3.at[nat._0]
      assertTypedEquals[Option[I]](Some(1), r3)


      val r4 = in2.at[nat._1]
      assertTypedEquals[Option[S]](None, r4)

      val r5 = in3.at[nat._1]
      assertTypedEquals[Option[S]](None, r5)

      val r6 = in3.at[nat._2]
      assertTypedEquals[Option[D]](None, r6)
    }
  }

  @Test
  def testPartition {
    type S = String; type I = Int; type D = Double; type C = Char
    val i   = Coproduct[I :+: CNil](1)
    val is   = Coproduct[I :+: S :+: CNil](1)

    val isdi: I :+: S :+: D :+: I :+: CNil =
      Inr[I, S :+: D :+: I :+: CNil](Coproduct[S :+: D :+: I :+: CNil](2))

    val r1 = i.partition[I]
    assertTypedEquals[Either[I :+: CNil, CNil]](Left(i), r1)
    val r2 = is.partition[I]
    assertTypedEquals[Either[I :+: CNil, S :+: CNil]](Left(i), r2)

    val r3 = i.partition[S]
    assertTypedEquals[Either[CNil, I :+: CNil]](Right(i), r3)
    val r4 = is.partition[S]
    assertTypedEquals[Either[S :+: CNil, I :+: CNil]](Right(i), r4)

    val r5 = i.partition[C]
    assertTypedEquals[Either[CNil, I :+: CNil]](Right(i), r5)
    val r6 = is.partition[C]
    assertTypedEquals[Either[CNil, I :+: S :+: CNil]](Right(is), r6)

    val r7 = isdi.partition[I]
    assertTypedEquals[Either[I :+: I :+: CNil, S :+: D :+: CNil]](Left(Inr[I, I :+: CNil](Inl[I, CNil](2))), r7)
  }

  @Test
  def testPartitionC {
    type S = String; type I = Int; type D = Double; type C = Char
    val i   = Coproduct[I :+: CNil](1)
    val is   = Coproduct[I :+: S :+: CNil](1)

    val isdi: I :+: S :+: D :+: I :+: CNil =
      Inr[I, S :+: D :+: I :+: CNil](Coproduct[S :+: D :+: I :+: CNil](2))

    val r1 = i.partitionC[I]
    assertTypedEquals[(I :+: CNil) :+: CNil :+: CNil](Inl(i), r1)
    val r2 = is.partitionC[I]
    assertTypedEquals[(I :+: CNil) :+: (S :+: CNil) :+: CNil](Inl(i), r2)

    val r3 = i.partitionC[S]
    assertTypedEquals[CNil :+: (I :+: CNil) :+: CNil](Inr(Inl(i)), r3)
    val r4 = is.partitionC[S]
    assertTypedEquals[(S :+: CNil) :+: (I :+: CNil) :+: CNil](Inr(Inl(i)), r4)

    val r5 = i.partitionC[C]
    assertTypedEquals[CNil :+: (I :+: CNil) :+: CNil](Inr(Inl(i)), r5)
    val r6 = is.partitionC[C]
    assertTypedEquals[CNil :+: (I :+: S :+: CNil) :+: CNil](Inr(Inl(is)), r6)

    val r7 = isdi.partitionC[I]
    assertTypedEquals[(I :+: I :+: CNil) :+: (S :+: D :+: CNil) :+: CNil](
      Inl(Inr[I, I :+: CNil](Inl[I, CNil](2))), r7)
  }

  @Test
  def testFilter {
    type S = String; type I = Int; type D = Double; type C = Char
    val i   = Coproduct[I :+: CNil](1)
    val is   = Coproduct[I :+: S :+: CNil](1)

    val isdi: I :+: S :+: D :+: I :+: CNil =
      Inr[I, S :+: D :+: I :+: CNil](Coproduct[S :+: D :+: I :+: CNil](2))

    val r1 = i.filter[I]
    assertTypedEquals[Option[I :+: CNil]](Some(i), r1)
    val r2 = is.filter[I]
    assertTypedEquals[Option[I :+: CNil]](Some(i), r2)

    val r3 = i.filter[S]
    assertTypedEquals[Option[CNil]](None, r3)
    val r4 = is.filter[S]
    assertTypedEquals[Option[S :+: CNil]](None, r4)

    val r5 = i.filter[C]
    assertTypedEquals[Option[CNil]](None, r5)
    val r6 = is.filter[C]
    assertTypedEquals[Option[CNil]](None, r6)

    val r7 = isdi.filter[I]
    assertTypedEquals[Option[I :+: I :+: CNil]](Some(Inr[I, I :+: CNil](Inl[I, CNil](2))), r7)
  }

  @Test
  def testFilterNot {
    type S = String; type I = Int; type D = Double; type C = Char
    val i     = Coproduct[I :+: CNil](1)
    val is    = Coproduct[I :+: S :+: CNil](1)

    val isdi: I :+: S :+: D :+: I :+: CNil =
      Inr[I, S :+: D :+: I :+: CNil](Coproduct[S :+: D :+: I :+: CNil](2))

    val r1 = i.filterNot[I]
    assertTypedEquals[Option[CNil]](None, r1)
    val r2 = is.filterNot[I]
    assertTypedEquals[Option[S :+: CNil]](None, r2)


    val r4 = i.filterNot[S]
    assertTypedEquals[Option[I :+: CNil]](Some(i), r4)
    val r5 = is.filterNot[S]
    assertTypedEquals[Option[I :+: CNil]](Some(i), r5)


    val r7 = i.filterNot[D]
    assertTypedEquals[Option[I :+: CNil]](Some(i), r7)
    val r8 = is.filterNot[D]
    assertTypedEquals[Option[I :+: S :+: CNil]](Some(is), r8)

    val r14 = isdi.filterNot[I]
    assertTypedEquals[Option[S :+: D :+: CNil]](None, r14)
  }

  @Test
  def testSplit {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val is = Coproduct[I :+: S :+: CNil](1)
    val dc = Coproduct[D :+: C :+: CNil](2.0)
    val isd = Coproduct[I :+: S :+: D :+: CNil](1)
    val isdc = Coproduct[I :+: S :+: D :+: C :+: CNil](2.0)

    {
      val r1 = in1.split(0)
      assertTypedEquals[Either[CNil, I :+: CNil]](Right(in1), r1)

      val r2 = is.split(0)
      assertTypedEquals[Either[CNil, I :+: S :+: CNil]](Right(is), r2)


      val r3 = in1.split(1)
      assertTypedEquals[Either[I :+: CNil, CNil]](Left(in1), r3)

      val r4 = is.split(1)
      assertTypedEquals[Either[I :+: CNil, S :+: CNil]](Left(in1), r4)

      val r5 = isd.split(1)
      assertTypedEquals[Either[I :+: CNil, S :+: D :+: CNil]](Left(in1), r5)


      // Cannot split at index 2 a coproduct of length 1
      illTyped(""" in1.split(2) """)

      val r7 = is.split(2)
      assertTypedEquals[Either[I :+: S :+: CNil, CNil]](Left(is), r7)

      val r8 = isd.split(2)
      assertTypedEquals[Either[I :+: S :+: CNil, D :+: CNil]](Left(is), r8)

      val r9 = isdc.split(2)
      assertTypedEquals[Either[I :+: S :+: CNil, D :+: C :+: CNil]](Right(dc), r9)
    }

    {
      val r1 = in1.split[_0]
      assertTypedEquals[Either[CNil, I :+: CNil]](Right(in1), r1)

      val r2 = is.split[_0]
      assertTypedEquals[Either[CNil, I :+: S :+: CNil]](Right(is), r2)


      val r3 = in1.split[_1]
      assertTypedEquals[Either[I :+: CNil, CNil]](Left(in1), r3)

      val r4 = is.split[_1]
      assertTypedEquals[Either[I :+: CNil, S :+: CNil]](Left(in1), r4)

      val r5 = isd.split[_1]
      assertTypedEquals[Either[I :+: CNil, S :+: D :+: CNil]](Left(in1), r5)


      // Cannot split at index 2 a coproduct of length 1
      illTyped(""" in1.split[_2] """)

      val r7 = is.split[_2]
      assertTypedEquals[Either[I :+: S :+: CNil, CNil]](Left(is), r7)

      val r8 = isd.split[_2]
      assertTypedEquals[Either[I :+: S :+: CNil, D :+: CNil]](Left(is), r8)

      val r9 = isdc.split[_2]
      assertTypedEquals[Either[I :+: S :+: CNil, D :+: C :+: CNil]](Right(dc), r9)
    }
  }

  @Test
  def testSplitC {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val is = Coproduct[I :+: S :+: CNil](1)
    val dc = Coproduct[D :+: C :+: CNil](2.0)
    val isd = Coproduct[I :+: S :+: D :+: CNil](1)
    val isdc = Coproduct[I :+: S :+: D :+: C :+: CNil](2.0)

    {
      val r1 = in1.splitC(0)
      assertTypedEquals[CNil :+: (I :+: CNil) :+: CNil](
        Coproduct[CNil :+: (I :+: CNil) :+: CNil](in1), r1)

      val r2 = is.splitC(0)
      assertTypedEquals[CNil :+: (I :+: S :+: CNil) :+: CNil](
        Coproduct[CNil :+: (I :+: S :+: CNil) :+: CNil](is), r2)


      val r3 = in1.splitC(1)
      assertTypedEquals[(I :+: CNil) :+: CNil :+: CNil](
        Coproduct[(I :+: CNil) :+: CNil :+: CNil](in1), r3)

      val r4 = is.splitC(1)
      assertTypedEquals[(I :+: CNil) :+: (S :+: CNil) :+: CNil](
        Coproduct[(I :+: CNil) :+: (S :+: CNil) :+: CNil](in1), r4)

      val r5 = isd.splitC(1)
      assertTypedEquals[(I :+: CNil) :+: (S :+: D :+: CNil) :+: CNil](
        Coproduct[(I :+: CNil) :+: (S :+: D :+: CNil) :+: CNil](in1), r5)


      // Cannot split at index 2 a coproduct of length 1
      illTyped(""" in1.splitC(2) """)

      val r7 = is.splitC(2)
      assertTypedEquals[(I :+: S :+: CNil) :+: CNil :+: CNil](
        Coproduct[(I :+: S :+: CNil) :+: CNil :+: CNil](is), r7)

      val r8 = isd.splitC(2)
      assertTypedEquals[(I :+: S :+: CNil) :+: (D :+: CNil) :+: CNil](
        Coproduct[(I :+: S :+: CNil) :+: (D :+: CNil) :+: CNil](is), r8)

      val r9 = isdc.splitC(2)
      assertTypedEquals[(I :+: S :+: CNil) :+: (D :+: C :+: CNil) :+: CNil](
        Coproduct[(I :+: S :+: CNil) :+: (D :+: C :+: CNil) :+: CNil](dc), r9)
    }

    {
      val r1 = in1.splitC[_0]
      assertTypedEquals[CNil :+: (I :+: CNil) :+: CNil](
        Coproduct[CNil :+: (I :+: CNil) :+: CNil](in1), r1)

      val r2 = is.splitC[_0]
      assertTypedEquals[CNil :+: (I :+: S :+: CNil) :+: CNil](
        Coproduct[CNil :+: (I :+: S :+: CNil) :+: CNil](is), r2)


      val r3 = in1.splitC[_1]
      assertTypedEquals[(I :+: CNil) :+: CNil :+: CNil](
        Coproduct[(I :+: CNil) :+: CNil :+: CNil](in1), r3)

      val r4 = is.splitC[_1]
      assertTypedEquals[(I :+: CNil) :+: (S :+: CNil) :+: CNil](
        Coproduct[(I :+: CNil) :+: (S :+: CNil) :+: CNil](in1), r4)

      val r5 = isd.splitC[_1]
      assertTypedEquals[(I :+: CNil) :+: (S :+: D :+: CNil) :+: CNil](
        Coproduct[(I :+: CNil) :+: (S :+: D :+: CNil) :+: CNil](in1), r5)


      // Cannot split at index 2 a coproduct of length 1
      illTyped(""" in1.splitC[_2] """)

      val r7 = is.splitC[_2]
      assertTypedEquals[(I :+: S :+: CNil) :+: CNil :+: CNil](
        Coproduct[(I :+: S :+: CNil) :+: CNil :+: CNil](is), r7)

      val r8 = isd.splitC[_2]
      assertTypedEquals[(I :+: S :+: CNil) :+: (D :+: CNil) :+: CNil](
        Coproduct[(I :+: S :+: CNil) :+: (D :+: CNil) :+: CNil](is), r8)

      val r9 = isdc.splitC[_2]
      assertTypedEquals[(I :+: S :+: CNil) :+: (D :+: C :+: CNil) :+: CNil](
        Coproduct[(I :+: S :+: CNil) :+: (D :+: C :+: CNil) :+: CNil](dc), r9)
    }
  }

  @Test
  def testTake {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val is = Coproduct[I :+: S :+: CNil](1)
    val dc = Coproduct[D :+: C :+: CNil](2.0)
    val isd = Coproduct[I :+: S :+: D :+: CNil](1)
    val isdc = Coproduct[I :+: S :+: D :+: C :+: CNil](2.0)

    {
      val r1 = in1.take(0)
      assertTypedEquals[Option[CNil]](None, r1)

      val r2 = is.take(0)
      assertTypedEquals[Option[CNil]](None, r2)


      val r3 = in1.take(1)
      assertTypedEquals[Option[I :+: CNil]](Some(in1), r3)

      val r4 = is.take(1)
      assertTypedEquals[Option[I :+: CNil]](Some(in1), r4)

      val r5 = isd.take(1)
      assertTypedEquals[Option[I :+: CNil]](Some(in1), r5)


      // Cannot take 2 elements out of a coproduct of length 1
      illTyped(""" in1.take(2) """)

      val r7 = is.take(2)
      assertTypedEquals[Option[I :+: S :+: CNil]](Some(is), r7)

      val r8 = isd.take(2)
      assertTypedEquals[Option[I :+: S :+: CNil]](Some(is), r8)

      val r9 = isdc.take(2)
      assertTypedEquals[Option[I :+: S :+: CNil]](None, r9)
    }

    {
      val r1 = in1.take[_0]
      assertTypedEquals[Option[CNil]](None, r1)

      val r2 = is.take[_0]
      assertTypedEquals[Option[CNil]](None, r2)


      val r3 = in1.take[_1]
      assertTypedEquals[Option[I :+: CNil]](Some(in1), r3)

      val r4 = is.take[_1]
      assertTypedEquals[Option[I :+: CNil]](Some(in1), r4)

      val r5 = isd.take[_1]
      assertTypedEquals[Option[I :+: CNil]](Some(in1), r5)


      // Cannot take 2 elements out of a coproduct of length 1
      illTyped(""" in1.take[_2] """)

      val r7 = is.take[_2]
      assertTypedEquals[Option[I :+: S :+: CNil]](Some(is), r7)

      val r8 = isd.take[_2]
      assertTypedEquals[Option[I :+: S :+: CNil]](Some(is), r8)

      val r9 = isdc.take[_2]
      assertTypedEquals[Option[I :+: S :+: CNil]](None, r9)
    }
  }

  @Test
  def testDrop {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val is = Coproduct[I :+: S :+: CNil](1)
    val dc = Coproduct[D :+: C :+: CNil](2.0)
    val isd = Coproduct[I :+: S :+: D :+: CNil](1)
    val isdc = Coproduct[I :+: S :+: D :+: C :+: CNil](2.0)

    {
      val r1 = in1.drop(0)
      assertTypedEquals[Option[I :+: CNil]](Some(in1), r1)

      val r2 = is.drop(0)
      assertTypedEquals[Option[I :+: S :+: CNil]](Some(is), r2)


      val r3 = in1.drop(1)
      assertTypedEquals[Option[CNil]](None, r3)

      val r4 = is.drop(1)
      assertTypedEquals[Option[S :+: CNil]](None, r4)

      val r5 = isd.drop(1)
      assertTypedEquals[Option[S :+: D :+: CNil]](None, r5)


      // Cannot drop 2 elements out of a coproduct of length 1
      illTyped(""" in1.drop(2) """)

      val r7 = is.drop(2)
      assertTypedEquals[Option[CNil]](None, r7)

      val r8 = isd.drop(2)
      assertTypedEquals[Option[D :+: CNil]](None, r8)

      val r9 = isdc.drop(2)
      assertTypedEquals[Option[D :+: C :+: CNil]](Some(dc), r9)
    }

    {
      val r1 = in1.drop[_0]
      assertTypedEquals[Option[I :+: CNil]](Some(in1), r1)

      val r2 = is.drop[_0]
      assertTypedEquals[Option[I :+: S :+: CNil]](Some(is), r2)


      val r3 = in1.drop[_1]
      assertTypedEquals[Option[CNil]](None, r3)

      val r4 = is.drop[_1]
      assertTypedEquals[Option[S :+: CNil]](None, r4)

      val r5 = isd.drop[_1]
      assertTypedEquals[Option[S :+: D :+: CNil]](None, r5)


      // Cannot drop 2 elements out of a coproduct of length 1
      illTyped(""" in1.drop[_2] """)

      val r7 = is.drop[_2]
      assertTypedEquals[Option[CNil]](None, r7)

      val r8 = isd.drop[_2]
      assertTypedEquals[Option[D :+: CNil]](None, r8)

      val r9 = isdc.drop[_2]
      assertTypedEquals[Option[D :+: C :+: CNil]](Some(dc), r9)
    }
  }

  @Test
  def testRemoveElem {
    type S = String; type I = Int; type D = Double; type C = Char
    val i = Coproduct[I :+: CNil](1)
    val is = Coproduct[I :+: S :+: CNil](1)
    val ii = Coproduct[I :+: I :+: CNil](1)

    val r1 = i.removeElemC[I]
    assertTypedEquals[I :+: CNil](i, r1)

    val r2 = i.removeElem[I]
    assertTypedEquals[Either[I, CNil]](Left(1), r2)

    val r3 = is.removeElemC[I]
    assertTypedEquals[I :+: S :+: CNil](is, r3)

    val r4 = is.removeElem[I]
    assertTypedEquals[Either[I, S :+: CNil]](Left(1), r4)

    val r5 = is.removeElemC[S]
    assertTypedEquals[S :+: I :+: CNil](Coproduct[S :+: I :+: CNil](1), r5)

    val r6 = is.removeElem[S]
    assertTypedEquals[Either[S, I :+: CNil]](Right(i), r6)

    // See https://github.com/milessabin/shapeless/issues/251
    val r7 = ii.removeElemC[I]
    assertTypedEquals[I :+: I :+: CNil](ii, r7)

    val r8 = ii.removeElem[I]
    assertTypedEquals[Either[I, I :+: CNil]](Left(1), r8)
  }

  @Test
  def testRemoveInverse = {
    type S = String; type I = Int; type D = Double; type C = Char
    val i = Coproduct[I :+: CNil](1)
    val is = Coproduct[I :+: S :+: CNil](1)
    val is0 = Coproduct[I :+: S :+: CNil]("a")
    val iis = Coproduct[I :+: S :+: I :+: CNil](2)
    val iis0 = Coproduct[I :+: S :+: I :+: CNil]("b")

    val u1 = Remove[I :+: CNil, I]
    val r1 = u1.inverse(Left(1))
    assertTypedEquals[I :+: CNil](i, r1)

    val u2 = Remove[I :+: S :+: CNil, I]
    val r2 = u2.inverse(Left(1))
    assertTypedEquals[I :+: S :+: CNil](is, r2)

    val r2_0 = u2.inverse(Right(Inl("a")))
    assertTypedEquals[I :+: S :+: CNil](is0, r2_0)

    val u3 = Remove[I :+: S :+: I :+: CNil, I]
    val r3 = u3.inverse(Left(2))
    assertTypedEquals[I :+: S :+: I :+: CNil](iis, r3)

    val r3_0 = u3.inverse(Right(Inl("b")))
    assertTypedEquals[I :+: S :+: I :+: CNil](iis0, r3_0)
  }

  @Test
  def testRemoveLastInverse = {
    type S = String; type I = Int; type D = Double; type C = Char
    val i = Coproduct[I :+: CNil](1)
    val is = Coproduct[I :+: S :+: CNil](1)
    val is0 = Coproduct[I :+: S :+: CNil]("a")
    val iis: I :+: S :+: I :+: CNil = Inr(Inr(Inl(2)))
    val iis0 = Coproduct[I :+: S :+: I :+: CNil]("b")

    val u1 = RemoveLast[I :+: CNil, I]
    val r1 = u1.inverse(Left(1))
    assertTypedEquals[I :+: CNil](i, r1)

    val u2 = RemoveLast[I :+: S :+: CNil, I]
    val r2 = u2.inverse(Left(1))
    assertTypedEquals[I :+: S :+: CNil](is, r2)

    val r2_0 = u2.inverse(Right(Inl("a")))
    assertTypedEquals[I :+: S :+: CNil](is0, r2_0)

    // These two are different from testRemoveInverse

    val u3 = RemoveLast[I :+: S :+: I :+: CNil, I]
    val r3 = u3.inverse(Left(2))
    assertTypedEquals[I :+: S :+: I :+: CNil](iis, r3)

    val r3_0 = u3.inverse(Right(Inr(Inl("b"))))
    assertTypedEquals[I :+: S :+: I :+: CNil](iis0, r3_0)
  }

  @Test
  def testToHList {
    type CISB = Int :+: String :+: Boolean :+: CNil
    type PISBa = Int :: String :: Boolean :: HNil
    type PISBb = the.`ToHList[CISB]`.Out
    implicitly[PISBa =:= PISBb]
  }

  @Test
  def testEmbedDeembed {
    type S1 = Int :+: CNil
    type S2 = Int :+: String :+: CNil
    type S3 = Int :+: String :+: Boolean :+: CNil
    type S4 = String :+: Boolean :+: CNil
    type S5 = Int :+: Int :+: Int :+: CNil

    val c1_0 = Coproduct[S1](5)
    val c1_1 = c1_0.embed[S2]
    assertTypedEquals[S2](c1_1, Coproduct[S2](5))
    assertTypedEquals[S1](c1_0, c1_1.deembed[S1].right.get)

    val c1_2 = c1_0.embed[S3]
    assertTypedEquals[S3](c1_2, Coproduct[S3](5))
    assertTypedEquals[S1](c1_0, c1_2.deembed[S1].right.get)

    val c2_0 = Coproduct[S2]("toto")
    val c2 = c2_0.embed[S3]
    assertTypedEquals[S3](c2, Coproduct[S3]("toto"))
    assertTypedEquals[S2](c2_0, c2.deembed[S2].right.get)

    illTyped("Coproduct[S1](5).embed[S4]")

    // See https://github.com/milessabin/shapeless/issues/253
    illTyped("Coproduct[S5](3).embed[S1]")

    // See https://github.com/milessabin/shapeless/issues/253#issuecomment-59648119
    {
      type II = Int :+: Int :+: CNil
      type IDI = Int :+: Double :+: Int :+: CNil

      val c1: II = Inr(Inl(1))
      val c2: II = Inl(1)

      val c1_0 = c1.embed[IDI].deembed[II].right.get
      val c2_0 = c2.embed[IDI].deembed[II].right.get

      assertTypedEquals[II](c1, c1_0)
      assertTypedEquals[II](c2, c2_0)
      assert(c2 != c1_0)
    }
  }

  @Test
  def testCoproductTypeSelector {
    import syntax.singleton._

    {
      type C = Coproduct.` `.T

      implicitly[C =:= CNil]
    }

    {
      type C = Coproduct.`Int`.T

      typed[C](Inl(23))
    }

    {
      type C = Coproduct.`Int, String`.T

      typed[C](Inl(23))
      typed[C](Inr(Inl("foo")))
    }

    {
      type C = Coproduct.`Int, String, Boolean`.T

      typed[C](Inl(23))
      typed[C](Inr(Inl("foo")))
      typed[C](Inr(Inr(Inl(true))))
    }

    // Literal types

    {
      type C = Coproduct.`2`.T

      typed[C](Inl(2.narrow))
    }

    {
      type C = Coproduct.`2, "a", true`.T

      typed[C](Inl(2.narrow))
      typed[C](Inr(Inl("a".narrow)))
      typed[C](Inr(Inr(Inl(true.narrow))))
    }

    {
      type C = Coproduct.`2`.T

      illTyped(""" typed[C](Inl(3.narrow)) """)
      ()
    }

    // Mix of standard and literal types

    {
      type C = Coproduct.`2, String, true`.T

      typed[C](Inl(2.narrow))
      typed[C](Inr(Inl("a")))
      typed[C](Inr(Inr(Inl(true.narrow))))
    }
  }
}

package CoproductTestAux {
  // See https://github.com/milessabin/shapeless/issues/328
  case class Foo(msg: String)
  case class Bar(msg: String)

  object Stuff {

    type Blah = Foo :+: Bar :+: Int :+: CNil
    val t = Coproduct[Blah](Foo("hi"))
    t.select[Foo]
  }
}
