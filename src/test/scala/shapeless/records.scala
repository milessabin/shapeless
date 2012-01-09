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

class RecordTests {
  
  import HList._

  def typed[T](t : => T) {}

  object intField1 extends Field[Int]
  object intField2 extends Field[Int]
  object stringField1 extends Field[String]
  object stringField2 extends Field[String]
  object boolField1 extends Field[Boolean]
  object boolField2 extends Field[Boolean]
  object doubleField1 extends Field[Double]
  object doubleField2 extends Field[Double]

  @Test
  def testRecords {
    val r1 =
      (intField1    ->    23) ::
      (stringField1 -> "foo") ::
      (boolField1   ->  true) ::
      (doubleField1 ->   2.0) ::
      HNil
    
    val v1 = r1(intField1)
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1(stringField1)
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r1(boolField1)
    typed[Boolean](v3)
    assertEquals(true, v3)
    
    val v4 = r1(doubleField1)
    typed[Double](v4)
    assertEquals(2.0, v4, Double.MinPositiveValue)

    val r2 = r1.updated(intField1, 7)
    val v5 = r2(intField1)
    typed[Int](v5)
    assertEquals(7, v5)
    
    val r3 = r1.updated(stringField1, "wibble")
    val v6 = r3(stringField1)
    typed[String](v6)
    assertEquals("wibble", v6)
    
    val r4 = r1.updated(boolField1, false)
    val v7 = r4(boolField1)
    typed[Boolean](v7)
    assertEquals(false, v7)
    
    val r5 = r1.updated(doubleField1, 1.0)
    val v8 = r5(doubleField1)
    typed[Double](v8)
    assertEquals(1.0, v8, Double.MinPositiveValue)

    val r6 = 
      (intField2    ->    13) ::
      (stringField2 -> "bar") ::
      r1

    val v9 = r6(intField2)
    typed[Int](v9)
    assertEquals(13, v9)
    
    val v10 = r6(stringField2)
    typed[String](v10)
    assertEquals("bar", v10)
    
    val r7 = HNil
    
    val ua = implicitly[UpdaterAux[HNil, boolField2.type, (boolField2.type, Boolean) :: HNil]]
    val u = Updater.updater[HNil, boolField2.type, (boolField2.type, Boolean) :: HNil]
    
    //val u2 = implicitly[Updater[HNil, boolField2.type] { type Out = (boolField2.type, Boolean) :: HNil}]
    //val u2b = implicitly[Updater[HNil, boolField2.type]]
    
    //val r8 = r7.updated(boolField2, false)
//
//    val v7 = r3(boolField2)
//    typed[Boolean](v7)
//    assertEquals(false, v7)
//    
//    val r4 = r3.updated(doubleField2, 3.0)
//    
//    val v8 = r4(doubleField2)
//    typed[Double](v8)
//    assertEquals(3.0, v8, Double.MinPositiveValue)
  }
}