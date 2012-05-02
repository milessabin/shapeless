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
  import Record._

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
  def testGet {
    val r1 =
      (intField1    ->    23) ::
      (stringField1 -> "foo") ::
      (boolField1   ->  true) ::
      (doubleField1 ->   2.0) ::
      HNil
    
    val v1 = r1.get(intField1)
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1.get(stringField1)
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r1.get(boolField1)
    typed[Boolean](v3)
    assertEquals(true, v3)
    
    val v4 = r1.get(doubleField1)
    typed[Double](v4)
    assertEquals(2.0, v4, Double.MinPositiveValue)
  }
  
  @Test
  def testUpdate {
    val r1 =
      (intField1    ->    23) ::
      (stringField1 -> "foo") ::
      (boolField1   ->  true) ::
      (doubleField1 ->   2.0) ::
      HNil

    val r2 = r1.updated(intField1, 7)
    val v1 = r2.get(intField1)
    typed[Int](v1)
    assertEquals(7, v1)
    
    val r3 = r1.updated(stringField1, "wibble")
    val v2 = r3.get(stringField1)
    typed[String](v2)
    assertEquals("wibble", v2)
    
    val r4 = r1.updated(boolField1, false)
    val v3 = r4.get(boolField1)
    typed[Boolean](v3)
    assertEquals(false, v3)
    
    val r5 = r1.updated(doubleField1, 1.0)
    val v4 = r5.get(doubleField1)
    typed[Double](v4)
    assertEquals(1.0, v4, Double.MinPositiveValue)
    
    val r6 = HNil

    val r7 = r6.updated(boolField2, false)
    val v5 = r7.get(boolField2)
    typed[Boolean](v5)
    assertEquals(false, v5)
    
    val r8 = r7.updated(doubleField2, 3.0)
    val v6 = r8.get(doubleField2)
    typed[Double](v6)
    assertEquals(3.0, v6, Double.MinPositiveValue)
  }
  
  @Test
  def testConcatenate {
    val r1 =
      (intField1    ->    23) ::
      (stringField1 -> "foo") ::
      (boolField1   ->  true) ::
      (doubleField1 ->   2.0) ::
      HNil

    val r2 = 
      (intField2    ->    13) ::
      (stringField2 -> "bar") ::
      r1

    val v1 = r2.get(intField2)
    typed[Int](v1)
    assertEquals(13, v1)
    
    val v2 = r2.get(stringField2)
    typed[String](v2)
    assertEquals("bar", v2)
  }
  
  @Test
  def testAppend {
    val r1 =
      (intField1    ->    23) ::
      (stringField1 -> "foo") ::
      HNil
      
    val r2 = r1 + (boolField1 -> true)
    typed[FieldEntry[intField1.type] :: FieldEntry[stringField1.type] :: FieldEntry[boolField1.type] :: HNil](r2)
    assertEquals((intField1 -> 23) :: (stringField1 -> "foo") :: (boolField1 -> true) :: HNil, r2)
    
    val r3 = r2 + (doubleField1 -> 2.0)
    typed[FieldEntry[intField1.type] :: FieldEntry[stringField1.type] :: FieldEntry[boolField1.type] :: FieldEntry[doubleField1.type] :: HNil](r3)
    assertEquals((intField1 -> 23) :: (stringField1 -> "foo") :: (boolField1 -> true) :: (doubleField1 -> 2.0) :: HNil, r3)
  }
  
  @Test
  def testRemove {
    import Record._
    
    val r1 =
      (intField1    ->    23) ::
      (stringField1 -> "foo") ::
      (boolField1   ->  true) ::
      (doubleField1 ->   2.0) ::
      HNil
      
    val rm1 = r1.remove(intField1)
    typed[(Int, FieldEntry[stringField1.type] :: FieldEntry[boolField1.type] :: FieldEntry[doubleField1.type] :: HNil)](rm1)
    assertEquals(23, rm1._1)
    assertEquals((stringField1 -> "foo") :: (boolField1 -> true) :: (doubleField1 -> 2.0) :: HNil, rm1._2)
    
    val rm2 = r1.remove(stringField1)
    typed[(String, FieldEntry[intField1.type] :: FieldEntry[boolField1.type] :: FieldEntry[doubleField1.type] :: HNil)](rm2)
    assertEquals("foo", rm2._1)
    assertEquals((intField1 -> 23) :: (boolField1 -> true) :: (doubleField1 -> 2.0) :: HNil, rm2._2)
    
    val rm3 = r1.remove(boolField1)
    typed[(Boolean, FieldEntry[intField1.type] :: FieldEntry[stringField1.type] :: FieldEntry[doubleField1.type] :: HNil)](rm3)
    assertEquals(true, rm3._1)
    assertEquals((intField1 -> 23) :: (stringField1 -> "foo") :: (doubleField1 -> 2.0) :: HNil, rm3._2)

    val rm4 = r1.remove(doubleField1)
    typed[(Double, FieldEntry[intField1.type] :: FieldEntry[stringField1.type] :: FieldEntry[boolField1.type] :: HNil)](rm4)
    assertEquals(2.0, rm4._1, Double.MinPositiveValue)
    assertEquals((intField1 -> 23) :: (stringField1 -> "foo") :: (boolField1 -> true) :: HNil, rm4._2)
    
    val r2 = r1 - intField1
    typed[FieldEntry[stringField1.type] :: FieldEntry[boolField1.type] :: FieldEntry[doubleField1.type] :: HNil](r2)
    assertEquals((stringField1 -> "foo") :: (boolField1 -> true) :: (doubleField1 -> 2.0) :: HNil, r2)

    val r3 = r1 - stringField1
    typed[FieldEntry[intField1.type] :: FieldEntry[boolField1.type] :: FieldEntry[doubleField1.type] :: HNil](r3)
    assertEquals((intField1 -> 23) :: (boolField1 -> true) :: (doubleField1 -> 2.0) :: HNil, r3)
    
    val r4 = r1 - boolField1
    typed[FieldEntry[intField1.type] :: FieldEntry[stringField1.type] :: FieldEntry[doubleField1.type] :: HNil](r4)
    assertEquals((intField1 -> 23) :: (stringField1 -> "foo") :: (doubleField1 -> 2.0) :: HNil, r4)
    
    val r5 = r1 - doubleField1
    typed[FieldEntry[intField1.type] :: FieldEntry[stringField1.type] :: FieldEntry[boolField1.type] :: HNil](r5)
    assertEquals((intField1 -> 23) :: (stringField1 -> "foo") :: (boolField1 -> true) :: HNil, r5)
  }
}
