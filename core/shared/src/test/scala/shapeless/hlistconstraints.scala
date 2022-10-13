/*
 * Copyright (c) 2011 Miles Sabin 
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

class HListConstraintsTests {

  trait Fruit
  case object Apple extends Fruit
  case object Pear extends Fruit

  @Test
  def testUnaryTCConstraint: Unit = {
    import UnaryTCConstraint._
    
    def acceptOption[L <: HList : *->*[Option]#λ](l : L) = true
    
    val l1 = Option(23) :: Option(true) :: Option("foo") :: HNil 
    val l2 = Option(23) :: true :: Option("foo") :: HNil
    
    acceptOption(l1)  // Compiles
    acceptOption(HNil: HNil)

    illTyped("""
    acceptOption(l2)
    """)

    val l3 = 23 :: true :: "foo" :: HNil 
    
    def acceptId[L <: HList : *->*[Id]#λ](l : L) = true

    acceptId(l3)  // Compiles
    acceptId(HNil: HNil)
    
    val l4 = "foo" :: "bar" :: "baz" :: HNil
    val l5 = "foo" :: true :: "baz" :: HNil
    
    def acceptConst[L <: HList : *->*[Const[String]#λ]#λ](l : L) = true
    
    acceptConst(l4)  // Compiles
    acceptConst(HNil: HNil)
    illTyped("""
    acceptConst(l5)
    """)

    def acceptTypeConstructor[F[_], L <: HList : *->*[F]#λ](l : L) = true

    acceptTypeConstructor(l1)  // Compiles - F = Option
    acceptTypeConstructor(l2)  // Compiles - F = Id
    acceptTypeConstructor(l3)  // Compiles - F = Id
    acceptTypeConstructor(l4)  // Compiles - F = Const[String]
    acceptTypeConstructor(l5)  // Compiles - F = Id
    acceptTypeConstructor(HNil: HNil)
  }
  
  @Test
  def testBasisConstraint: Unit = {
    import BasisConstraint._
    
    type M = Int :: Boolean :: String :: HNil
    
    def acceptBasis[L <: HList : Basis[M]#λ](l : L) = true
    
    val l1 = 23 :: true :: 13 :: 7 :: 5 :: false :: "foo" :: "bar" :: HNil
    val l2 = 23 :: true :: 13 :: 7 :: 5 :: 2.0 :: "foo" :: "bar" :: HNil

    acceptBasis(l1) // Compiles
    acceptBasis(HNil: HNil)
    illTyped("""
    acceptBasis(l2)
    """)
  }
  
  @Test
  def testLUBConstraint: Unit = {
    import LUBConstraint._
    
    def acceptLUB[L <: HList : <<:[Fruit]#λ](l : L) = true
    
    val l1 = Apple :: Pear :: Apple :: Pear :: HNil
    val l2 = Apple :: 23 :: "foo" :: Pear :: HNil

    acceptLUB(l1) // Compiles
    acceptLUB(HNil: HNil)
    illTyped("""
    acceptLUB(l2)
    """)
  }

  @Test
  def testKeyValueConstraints: Unit = {
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

    def acceptKeys[R <: HList : Keys[author.type :: title.type :: id.type :: HNil]#λ](r : R) = true
    
    acceptKeys(summary)   // Compiles
    acceptKeys(HNil: HNil)
    illTyped("""
    acceptKeys(book)
    """)

    def acceptValues[R <: HList : Values[Int :: String :: HNil]#λ](r : R) = true
    
    acceptValues(summary) // Compiles
    acceptValues(HNil: HNil)
    illTyped("""
    acceptValues(book)
    """)
  }

  @Test
  def testNotContainsConstraint: Unit = {

    import NotContainsConstraint._

    def notContains[L <: HList:NotContains[String]#λ, U](l: L, u: U)(implicit ev: NotContainsConstraint[L, U]) = true

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
  def testIsDistinctConstraint: Unit = {

    def isDistinct[L <: HList](l: L)(implicit ev: IsDistinctConstraint[L]) = true

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

}
