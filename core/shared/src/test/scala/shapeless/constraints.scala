/*
 * Copyright (c) 2011-2020 Miles Sabin 
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

trait Fruit
case object Apple extends Fruit
case object Pear extends Fruit

class UnaryTCConstraintTests{
  import UnaryTCConstraint._

  def acceptOption[T](l : T)(implicit ev: UnaryTCConstraint[T, Option]) = true
  def acceptId[T](l : T)(implicit ev: UnaryTCConstraint[T, Id]) = true
  def acceptConst[T](l : T)(implicit ev: UnaryTCConstraint[T, Const[String]#λ]) = true
  def acceptTypeConstructor[F[_], T](l : T)(implicit ev: UnaryTCConstraint[T, F]) = true

  @Test
  def testHList: Unit ={
    val l1 = Option(23) :: Option(true) :: Option("foo") :: HNil 
    val l2 = Option(23) :: true :: Option("foo") :: HNil

    acceptOption(l1)  // Compiles
    acceptOption(HNil: HNil)

    illTyped("""
    acceptOption(l2)
    """)

    val l3 = 23 :: true :: "foo" :: HNil 

    acceptId(l3)  // Compiles
    acceptId(HNil: HNil)

    val l4 = "foo" :: "bar" :: "baz" :: HNil
    val l5 = "foo" :: true :: "baz" :: HNil

    acceptConst(l4)  // Compiles
    acceptConst(HNil: HNil)
    illTyped("""
    acceptConst(l5)
    """)

    acceptTypeConstructor(l1)  // Compiles - F = Option
    acceptTypeConstructor(l2)  // Compiles - F = Id
    acceptTypeConstructor(l3)  // Compiles - F = Id
    acceptTypeConstructor(l4)  // Compiles - F = Const[String]
    acceptTypeConstructor(l5)  // Compiles - F = Id
    acceptTypeConstructor(HNil: HNil)
  }

  @Test
  def testTuple: Unit ={
    val l1 = (Option(23), Option(true), Option("foo"))
    val l2 = (Option(23), true, Option("foo"))

    acceptOption(l1)  // Compiles

    illTyped("""
    acceptOption(l2)
    """)

    val l3 = (23, true, "foo")

    acceptId(l3)  // Compiles

    val l4 = ("foo", "bar", "baz")
    val l5 = ("foo", true, "baz")

    acceptConst(l4)

    illTyped("""
    acceptConst(l5)
    """)

    acceptTypeConstructor(l1)  // Compiles - F = Option
    acceptTypeConstructor(l2)  // Compiles - F = Id
    acceptTypeConstructor(l3)  // Compiles - F = Id
    acceptTypeConstructor(l4)  // Compiles - F = Const[String]
    acceptTypeConstructor(l5)  // Compiles - F = Id
  }

  @Test
  def testCoproduct: Unit ={
    type L1 = Option[Int] :+: Option[Boolean] :+: Option[String] :+: CNil
    val l1: L1 = Coproduct[L1](Option(23))

    type L2 = Option[Int] :+: Boolean :+: Option[String] :+: CNil
    val l2: L2 = Coproduct[L2](Option("foo"))

    acceptOption(l1)  // Compiles

    illTyped("""
    acceptOption(l2)
    """)

    type L3 = Int :+: Boolean :+: String :+: CNil
    val l3: L3 = Coproduct[L3](23)

    acceptId(l3)  // Compiles

    type L4 = String :+: String :+: String :+: CNil
    val l4: L4 = Coproduct[L4]("baz")

    type L5 = String :+: Boolean :+: String :+: CNil
    val l5: L5 = Coproduct[L5](true)

    acceptConst(l4)  // Compiles

    illTyped("""
    acceptConst(l5)
    """)

    acceptTypeConstructor(l1)  // Compiles - F = Option
    acceptTypeConstructor(l2)  // Compiles - F = Id
    acceptTypeConstructor(l3)  // Compiles - F = Id
    acceptTypeConstructor(l4)  // Compiles - F = Const[String]
    acceptTypeConstructor(l5)  // Compiles - F = Id
  }

  case class Shout(one: Option[Int], two: Option[Int])
  case class Yell(one: String, two: String)
  case class Scream[A, B](one: A, two: B)

  @Test
  def testGeneric: Unit ={
    val l1 = Shout(None, None)
    val l2 = Yell("really", "loudly")

    acceptOption(l1)  // Compiles
    illTyped("""
    acceptOption(l2)
    """)

    acceptId(l1)
    acceptId(l2)

    val l3 = Scream(1, "two")

    //acceptConst(l2) //fails on oraclejdk8 2.11.x and 2.12.x
    illTyped("""
    acceptConst(l3)
    """)

    acceptTypeConstructor(l1)          // Compiles - F = Option
    acceptTypeConstructor(l2)          // Compiles - F = Const[String]
    acceptTypeConstructor(l3)          // Compiles - F = Id
  }
}

class BasisConstraintTests{
  import BasisConstraint._

  @Test
  def testHList: Unit ={  
    type M = Int :: Boolean :: String :: HNil
    def acceptBasis[L : Basis[M]#λ](l : L) = true

    val l1 = 23 :: true :: 13 :: 7 :: 5 :: false :: "foo" :: "bar" :: HNil
    val l2 = 23 :: true :: 13 :: 7 :: 5 :: 2.0 :: "foo" :: "bar" :: HNil

    acceptBasis(l1) // Compiles
    acceptBasis(HNil: HNil)
    illTyped("""
    acceptBasis(l2)
    """)
  }

  @Test
  def testTuple: Unit ={  
    type M = (Int, Boolean, String)
    def acceptBasis[L : Basis[M]#λ](l : L) = true

    val l1 = (23, true, 13, 7, 5, false, "foo", "bar")
    val l2 = (23, true, 13, 7, 5, 2.0, "foo", "bar")

    acceptBasis(l1) // Compiles
    illTyped("""
    acceptBasis(l2)
    """)
  }

  @Test
  def testCoproduct: Unit ={  
    type M = Int :+: Boolean :+: String :+: CNil
    def acceptBasis[L : Basis[M]#λ](l : L) = true

    val l1 = Coproduct[M]("foo")
    type L2 = Int :+: Boolean :+: Double :+: String :+: CNil
    val l2 = Coproduct[L2]("bar")

    acceptBasis(l1)         // Compiles
    illTyped("""
    acceptBasis(l2)
    """)
  }

  case class ThingOne(one: String)
  case class ThingTwo(two: String)
  case class SomethingElse(three: Int)

  @Test
  def testGeneric: Unit ={
    def acceptBasis[L : Basis[ThingOne]#λ](l : L) = true

    val l1 = ThingTwo("one")

    acceptBasis(l1)
    illTyped("""
    acceptBasis(SomethingElse(1))
    """)
  }
}

class LUBConstraintTests {
  import LUBConstraint._

  def acceptLUB[L : <<:[Fruit]#λ](l : L) = true

  @Test
  def testHList: Unit ={
    val l1 = Apple :: Pear :: Apple :: Pear :: HNil
    val l2 = Apple :: 23 :: "foo" :: Pear :: HNil

    acceptLUB(l1) // Compiles
    acceptLUB(HNil: HNil)
    illTyped("""
    acceptLUB(l2)
    """)
  }

  @Test
  def testTuple: Unit ={
    val l1 = (Apple, Pear, Apple, Pear)
    val l2 = (Apple, 23, "foo", Pear)

    acceptLUB(l1) // Compiles
    illTyped("""
    acceptLUB(l2)
    """)
  }

  @Test
  def testCoproduct: Unit ={
    type L1 = Apple.type :+: Pear.type :+: CNil
    val l1 = Coproduct[L1](Apple)

    type L2 = Apple.type :+: Int :+: String :+: Pear.type :+: CNil
    val l2 = Coproduct[L2](23)

    acceptLUB(l1) // Compiles
    illTyped("""
    acceptLUB(l2)
    """)
  }

  case class Juice[F <: Fruit](f: F)
  case class Mixed[F, G](f: F, g: G)

  @Test
  def testGeneric: Unit ={
    val l1 = Juice(Apple)
    val l2 = Mixed(Pear, 1)

    acceptLUB(l1) // Compiles
    illTyped("""
    acceptLUB(l2)
    """)
  }
}

class KeyValueConstraintTests {
  import KeyConstraint._
  import ValueConstraint._

  object author  extends FieldOf[String]
  object title   extends FieldOf[String]
  object id      extends FieldOf[Int]
  object price   extends FieldOf[Double]
  object inPrint extends FieldOf[Boolean]

  val book =
    (author ->> "Benjamin Pierce") ::
    (title  ->> "Types and Programming Languages") ::
    (id     ->>  262162091) ::
    (price  ->>  44.11) ::
    HNil
  
  val summary = 
    (author ->> "Benjamin Pierce") ::
    (title  ->> "Types and Programming Languages") ::
    (id     ->>  262162091) ::
    HNil

  @Test
  def testKeys: Unit = {
    def acceptKeys[R <: HList : Keys[author.type :: title.type :: id.type :: HNil]#λ](r : R) = true
    
    acceptKeys(summary)   // Compiles
    acceptKeys(HNil: HNil)
    illTyped("""
    acceptKeys(book)
    """)
  }

  @Test
  def testValues: Unit = {
    def acceptValues[R <: HList : Values[Int :: String :: HNil]#λ](r : R) = true
    
    acceptValues(summary) // Compiles
    acceptValues(HNil: HNil)
    illTyped("""
    acceptValues(book)
    """)
  }
}

class NotContainsConstraintTests {
  import NotContainsConstraint._

  def notContains[L:NotContains[String]#λ, U](l: L, u: U)(implicit ev: NotContainsConstraint[L, U]) = true

  @Test
  def testHList: Unit ={
    notContains(HNil: HNil, 2)
    notContains(2 :: HNil, "str")
    notContains(Pear :: 2 :: HNil, Apple)
    notContains(Pear :: 2 :: HNil, new Fruit{})

    illTyped("""
    notContains(2 :: HNil, 3)
    """)

    illTyped("""
    notContains("str" :: Pear :: 2 :: HNil, Pear)
    """)
  }

  @Test
  def testTuple: Unit ={
    notContains(2 -> 2.3, "2")
    notContains(Pear -> 2, Apple)

    illTyped("""
    notContains[Int](2 -> HNil, 1)
    """)

    illTyped("""
    notContains(("str", Pear, 2), Pear)
    """)
  }

  @Test
  def testHCoproduct: Unit ={
    type L1 = Int :+: CNil
    type L2 = Pear.type :+: Int :+: CNil

    notContains(Coproduct[L1](1), "1")
    notContains(Coproduct[L2](1), Apple)

    illTyped("""
    notContains(Coproduct[L1](1), 1)
    """)

    illTyped("""
    notContains(Coproduct[L2](Pear), Pear)
    """)
  }

  case class Box2[A](x: A, y: A)
  case class Box3[A, B](x: A, y: A, z: B)

  @Test
  def testGeneric: Unit ={
    notContains(Box2(1, 1), "1")
    notContains(Box3(Pear, Pear, 1), Apple)

    illTyped("""
    notContains(Box2(1, 1), 1)
    """)

    illTyped("""
    notContains(Box3(2, 1, Pear), Pear)
    """)
  }
}

class IsDistinctConstraintTests {
  def isDistinct[L](l: L)(implicit ev: IsDistinctConstraint[L]) = true

  @Test
  def testHList: Unit ={
    isDistinct(HNil: HNil)
    isDistinct(2 :: HNil)
    isDistinct("str" :: Pear :: Apple :: new Fruit{} :: 2 :: HNil)

    illTyped("""
    isDistinct(10 :: 2 :: HNil)
    """)

    illTyped("""
    isDistinct(10 :: "str" :: 5 :: HNil)
    """)

    illTyped("""
    isDistinct(Pear :: true :: "str" :: 2 :: false :: HNil)
    """)
  }

  @Test
  def testTuple: Unit ={
    isDistinct(2 -> HNil)
    isDistinct(("str", Pear, Apple, new Fruit{}, 2))

    illTyped("""
    isDistinct(10 -> 2)
    """)

    illTyped("""
    isDistinct((10, "str", 5))
    """)

    illTyped("""
    isDistinct((Pear, true, "str", 2, false))
    """)
  }

  @Test
  def testCoproduct: Unit ={
    type L1 = Int :+: CNil
    isDistinct(Coproduct[L1](2))

    type L2 = String :+: Pear.type :+: Apple.type :+: Fruit :+: Int :+: CNil
    isDistinct(Coproduct[L2]("str"))

    illTyped("""
    type L3 = Int :+: Int :+: CNil
    isDistinct(Coproduct[L3](1))
    """)

    illTyped("""
    type L4 = Int :+: String :+: Int :+: CNil
    isDistinct(Coproduct[L4]("str"))
    """)
  }

  case class Same[A](a1: A, a2: A)
  case class Diff[A, B](a: A, b: B)

  @Test
  def testGeneric: Unit ={
    isDistinct(Diff(1, 4.0))
    isDistinct(Diff(Pear, Apple))

    illTyped("""
    isDistinct(Same(1, 1))
    """)

    illTyped("""
    isDistinct(Same(Apple, Pear))
    """)

    illTyped("""
    isDistinct(Diff("1", "2"))
    """)
  }
} 