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
  def testSuperConstraint {
    type N = Int :: String :: HNil
    type M = Boolean :: N

    type Other = Double :: N

    val nCompatible = 23 :: 13 :: "foo" :: "bar" :: HNil
    val mCompatible = 'a' :: false :: nCompatible

    import SuperConstraint._

    def acceptM[L <: HList: Super[M]#λ](l: L) = {
      //val a: Boolean = wrap { acceptOther(l)(_: Other IsSuperOf L) } //won't compile, because Other is no Super HList of M      

      val b: Boolean = prove { acceptNExplicit(l) }
      val c: Boolean = prove { acceptNImplicit(l)(_: N IsSuperOf L) }
      c || b
    }

    def acceptNImplicit[L <: HList: Super[N]#λ](l: L) = true
    def acceptNExplicit[L <: HList](l: L)(ev: N IsSuperOf L) = true

    def acceptOther[L <: HList: Super[Other]#λ](l: L) = true

    //acceptM(nCompatible)//won't compile because M is no Super HList of nCompatible
    acceptM(mCompatible)

    acceptOther(2.0 :: mCompatible)
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

    acceptLUB(l1) // Compiles
    //acceptLUB(l2) // Does not compile
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
