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

class HListConstraintsTests {
  @Test
  def testFilterRel {
    import FilterRel._

    def accept[L1 <: HList, L2 <: HList](l1: L1)(l2: L2)(implicit f: *==*[Int]#λ[L1, L2]) = l2

    val l1 = 1 :: "a" :: 3 :: HNil
    val l2 = 1 :: 3 :: HNil

    accept(l1)(l2)
    //accept(l1)(l1) // Does not compile
  }

  @Test
  def testNatTRel {
    import TypeOperators._
    import NatTRel._

    {
      def acceptOptionToList[L1 <: HList, L2 <: HList](l1: L1)(l2: L2)(implicit e: (Option ~??> List)#λ[L1, L2]) = l2

      val l1 = Option(0) :: Option("a") :: HNil
      val l2 = List(1) :: List("b") :: HNil

      acceptOptionToList(l1)(l2)
      //acceptOptionToId(l1)(l1)  // Does not compile
    }

    {
      def acceptOptionToId[L1 <: HList, L2 <: HList](l1: L1)(l2: L2)(implicit e: (Option ~??> Id)#λ[L1, L2]) = l2

      val l1 = Option(0) :: Option("a") :: HNil
      val l2 = 1 :: "b" :: HNil

      acceptOptionToId(l1)(l2)
      //acceptOptionToId(l1)(l1)  // Does not compile
    }

    {
      def acceptIdToOption[L1 <: HList, L2 <: HList](l1: L1)(l2: L2)(implicit e: (Id ~??> Option)#λ[L1, L2]) = l2

      val l1 = 1 :: "b" :: HNil
      val l2 = Option(0) :: Option("a") :: HNil

      acceptIdToOption(l1)(l2)
      //acceptIdToOption(l1)(l1)  // Does not compile
    }
  }

  @Test
  def testUnaryTCConstraint {
    import TypeOperators._
    import UnaryTCConstraint._
    
    def acceptOption[L <: HList : *->*[Option]#λ](l : L) = true
    
    val l1 = Option(23) :: Option(true) :: Option("foo") :: HNil 
    val l2 = Option(23) :: true :: Option("foo") :: HNil
    
    acceptOption(l1)  // Compiles
    //acceptOption(l2)  // Does not compile

    val l3 = 23 :: true :: "foo" :: HNil 
    
    def acceptId[L <: HList : *->*[Id]#λ](l : L) = true

    acceptId(l3)  // Compiles
    
    val l4 = "foo" :: "bar" :: "baz" :: HNil
    val l5 = "foo" :: true :: "baz" :: HNil
    
    def acceptConst[L <: HList : *->*[Const[String]#λ]#λ](l : L) = true
    
    acceptConst(l4)  // Compiles
    //acceptConst(l5)  // Does not compile
  }
  
  @Test
  def testBasisConstraint {
    import BasisConstraint._
    
    type M = Int :: Boolean :: String :: HNil
    
    def acceptBasis[L <: HList : Basis[M]#λ](l : L) = true
    
    val l1 = 23 :: true :: 13 :: 7 :: 5 :: false :: "foo" :: "bar" :: HNil
    val l2 = 23 :: true :: 13 :: 7 :: 5 :: 2.0 :: "foo" :: "bar" :: HNil

    acceptBasis(l1) // Compiles
    //acceptBasis(l2) // Does not compile
  }
  
  @Test
  def testKeyValueConstraints {
    import KeyConstraint._
    import ValueConstraint._
    
    object author  extends Field[String]
    object title   extends Field[String]
    object id      extends Field[Int]
    object price   extends Field[Double]
    object inPrint extends Field[Boolean]

    val book =
      (author -> "Benjamin Pierce") ::
      (title  -> "Types and Programming Languages") ::
      (id     ->  262162091) ::
      (price  ->  44.11) ::
      HNil
    
    val summary = 
      (author -> "Benjamin Pierce") ::
      (title  -> "Types and Programming Languages") ::
      (id     ->  262162091) ::
      HNil

    def acceptKeys[R <: HList : Keys[author.type :: title.type :: id.type :: HNil]#λ](r : R) = true
    
    acceptKeys(summary)   // Compiles
    //acceptKeys(book)    // Does not compile

    def acceptValues[R <: HList : Values[Int :: String :: HNil]#λ](r : R) = true
    
    acceptValues(summary) // Compiles
    //acceptValues(book)  // Does not compile
  }
}
