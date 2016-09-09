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
}
