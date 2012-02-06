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
  def testSchemaConstraint {
    import SchemaConstraint._
    
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
      
    def acceptSummary[R <: HList : Schema[Int :: String :: HNil]#λ](r : R) = true
    
    acceptSummary(summary) // Compiles
    // acceptSummary(book) // Does not compiles
  }
}