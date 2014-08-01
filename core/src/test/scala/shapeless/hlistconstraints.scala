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
  @Test
  def testUnaryTCConstraint {
    import UnaryTCConstraint._
    
    def acceptOption[L <: HList : *->*[Option]#λ](l : L) = true
    
    val l1 = Option(23) :: Option(true) :: Option("foo") :: HNil 
    val l2 = Option(23) :: true :: Option("foo") :: HNil
    
    acceptOption(HNil) // Compiles
    acceptOption(l1)  // Compiles

    illTyped("""
    acceptOption(l2)
    """)

    val l3 = 23 :: true :: "foo" :: HNil 
    
    def acceptId[L <: HList : *->*[Id]#λ](l : L) = true

    acceptId(l3)  // Compiles
    
    val l4 = "foo" :: "bar" :: "baz" :: HNil
    val l5 = "foo" :: true :: "baz" :: HNil
    
    def acceptConst[L <: HList : *->*[Const[String]#λ]#λ](l : L) = true
    
    acceptConst(l4)  // Compiles
    illTyped("""
    acceptConst(l5)
    """)
  }
  
  @Test
  def testBasisConstraint {
    import BasisConstraint._
    
    type M = Int :: Boolean :: String :: HNil
    
    def acceptBasis[L <: HList : Basis[M]#λ](l : L) = true
    
    val l1 = 23 :: true :: 13 :: 7 :: 5 :: false :: "foo" :: "bar" :: HNil
    val l2 = 23 :: true :: 13 :: 7 :: 5 :: 2.0 :: "foo" :: "bar" :: HNil

    acceptBasis(HNil) // Compiles
    acceptBasis(l1) // Compiles
    illTyped("""
    acceptBasis(l2)
    """)
  }
  
  @Test
  def testLUBConstraint {
    import LUBConstraint._
    
    trait Fruit
    case object Apple extends Fruit
    case object Pear extends Fruit
    
    def acceptLUB[L <: HList : <<:[Fruit]#λ](l : L) = true
    
    val l1 = Apple :: Pear :: Apple :: Pear :: HNil
    val l2 = Apple :: 23 :: "foo" :: Pear :: HNil

    acceptLUB(HNil) // Compiles
    acceptLUB(l1) // Compiles
    illTyped("""
    acceptLUB(l2)
    """)
  }

  @Test
  def testKeyValueConstraints {
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
    
    acceptKeys(HNil)   // Compiles
    acceptKeys(summary)   // Compiles
    illTyped("""
    acceptKeys(book)
    """)

    def acceptValues[R <: HList : Values[Int :: String :: HNil]#λ](r : R) = true
    
    acceptValues(HNil) // Compiles
    acceptValues(summary) // Compiles
    illTyped("""
    acceptValues(book)
    """)
  }
}
