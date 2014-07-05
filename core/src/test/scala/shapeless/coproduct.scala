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

import ops.coproduct._

class CoproductTests {
  type ISB = Int :+: String :+: Boolean :+: CNil
  type III = Int :+: Int :+: Int :+: CNil
  
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
  }

  @Test
  def testWithKeys {
    import syntax.singleton._
    import record.RecordType
    import union._
    import ops.union._

    val uSchema = RecordType.like('i ->> 23 :: 's ->> "foo" :: 'b ->> true :: HNil)
    val cKeys = Keys[uSchema.Union].apply()

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
  }

  @Test
  def testPartialOrdering {
    val (one, two, abc, xyz) =
      (Coproduct[ISB](1), Coproduct[ISB](2), Coproduct[ISB]("abc"), Coproduct[ISB]("xyz"))

    def assertPOEquals(expected: Option[Int], l: ISB, r: ISB)(implicit po: PartialOrdering[ISB]) =
      assertEquals(s"${l} ${r}", expected, po.tryCompare(l, r))

    assertPOEquals(Some(0),  one, one)
    assertPOEquals(Some(-1), one, two)
    assertPOEquals(Some(1),  two, one)

    assertPOEquals(Some(0),  abc, abc)
    assertPOEquals(Some(-23), abc, xyz)
    assertPOEquals(Some(23),  xyz, abc)

    assertPOEquals(None, one, abc)
    assertPOEquals(None, abc, one)
  }

  @Test
  def testLength {
    assertTypedEquals[Nat._1](Nat._1, Coproduct[Int :+: CNil](123).length)
    assertTypedEquals[Nat._2](Nat._2, Coproduct[Int :+: String :+: CNil](123).length)
    assertTypedEquals[Nat._3](Nat._3, Coproduct[Int :+: String :+: Double :+: CNil](123).length)
    assertTypedEquals[Nat._4](Nat._4, Coproduct[Int :+: String :+: Double :+: Char :+: CNil](123).length)
  }

  @Test
  def testAppend {
    type S = String; type I = Int; type D = Double; type C = Char
    type CoI    = I :+: CNil
    type CoIS   = I :+: S :+: CNil
    type CoISD  = I :+: S :+: D :+: CNil
    type CoISDC = I :+: S :+: D :+: C :+: CNil

    assertTypedEquals[CoIS](Coproduct[CoIS](1), Coproduct[CoI](1).append[S])
    assertTypedEquals[CoISD](Coproduct[CoISD](1), Coproduct[CoIS](1).append[D])
    assertTypedEquals[CoISDC](Coproduct[CoISDC](1), Coproduct[CoISD](1).append[C])
  }

  @Test
  def testRotateLeft {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val in2 = Coproduct[I :+: S :+: CNil](1)
    val in3 = Coproduct[I :+: S :+: D :+: CNil](1)
    val in4 = Coproduct[I :+: S :+: D :+: C :+: CNil](1)

    // rotateLeft[_0]
    assertTypedSame[I :+: CNil](in1, in1.rotateLeft[_0])
    assertTypedSame[I :+: S :+: CNil](in2, in2.rotateLeft[_0])
    assertTypedSame[I :+: S :+: D :+: CNil](in3, in3.rotateLeft[_0])
    assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, in4.rotateLeft[_0])

    // rotateLeft[N % Size == 0]
    assertTypedSame[I :+: CNil](in1, in1.rotateLeft[_1])
    assertTypedSame[I :+: CNil](in1, in1.rotateLeft[_2])
    assertTypedSame[I :+: S :+: CNil](in2, in2.rotateLeft[_2])
    assertTypedSame[I :+: S :+: CNil](in2, in2.rotateLeft[_4])
    assertTypedSame[I :+: S :+: D :+: CNil](in3, in3.rotateLeft[_3])
    assertTypedSame[I :+: S :+: D :+: CNil](in3, in3.rotateLeft[_6])
    assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, in4.rotateLeft[_4])
    assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, in4.rotateLeft[_8])

    // other
    assertTypedEquals[S :+: I :+: CNil](Coproduct[S :+: I :+: CNil](1), in2.rotateLeft[_1])
    assertTypedEquals[S :+: D :+: I :+: CNil](Coproduct[S :+: D :+: I :+: CNil](1), in3.rotateLeft[_1])
    assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), in4.rotateLeft[_1])
    assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), in4.rotateLeft[_2])
    assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), in4.rotateLeft[_3])
    assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), in4.rotateLeft[_5])
    assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), in4.rotateLeft[_6])
  }

  @Test
  def testRotateRight {
    import Nat._
    type S = String; type I = Int; type D = Double; type C = Char
    val in1 = Coproduct[I :+: CNil](1)
    val in2 = Coproduct[I :+: S :+: CNil](1)
    val in3 = Coproduct[I :+: S :+: D :+: CNil](1)
    val in4 = Coproduct[I :+: S :+: D :+: C :+: CNil](1)

    // rotateRight[_0]
    assertTypedSame[I :+: CNil](in1, in1.rotateRight[_0])
    assertTypedSame[I :+: S :+: CNil](in2, in2.rotateRight[_0])
    assertTypedSame[I :+: S :+: D :+: CNil](in3, in3.rotateRight[_0])
    assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, in4.rotateRight[_0])

    // rotateRight[N % Size == 0]
    assertTypedSame[I :+: CNil](in1, in1.rotateRight[_1])
    assertTypedSame[I :+: CNil](in1, in1.rotateRight[_2])
    assertTypedSame[I :+: S :+: CNil](in2, in2.rotateRight[_2])
    assertTypedSame[I :+: S :+: CNil](in2, in2.rotateRight[_4])
    assertTypedSame[I :+: S :+: D :+: CNil](in3, in3.rotateRight[_3])
    assertTypedSame[I :+: S :+: D :+: CNil](in3, in3.rotateRight[_6])
    assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, in4.rotateRight[_4])
    assertTypedSame[I :+: S :+: D :+: C :+: CNil](in4, in4.rotateRight[_8])

    // other
    assertTypedEquals[S :+: I :+: CNil](Coproduct[S :+: I :+: CNil](1), in2.rotateRight[_1])
    assertTypedEquals[D :+: I :+: S :+: CNil](Coproduct[D :+: I :+: S :+: CNil](1), in3.rotateRight[_1])
    assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), in4.rotateRight[_1])
    assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), in4.rotateRight[_2])
    assertTypedEquals[S :+: D :+: C :+: I :+: CNil](Coproduct[S :+: D :+: C :+: I :+: CNil](1), in4.rotateRight[_3])
    assertTypedEquals[C :+: I :+: S :+: D :+: CNil](Coproduct[C :+: I :+: S :+: D :+: CNil](1), in4.rotateRight[_5])
    assertTypedEquals[D :+: C :+: I :+: S :+: CNil](Coproduct[D :+: C :+: I :+: S :+: CNil](1), in4.rotateRight[_6])
  }

  private def assertTypedEquals[A](expected: A, actual: A) = assertEquals(expected, actual)
  private def assertTypedSame[A](expected: A, actual: A) = assertSame(expected, actual)
}
