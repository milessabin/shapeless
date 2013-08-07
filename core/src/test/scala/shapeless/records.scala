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
  import Record._
  import SingletonTypes._

  def typed[T](t : => T) {}

  object intField1 //extends FieldT[Int]
  object intField2 //extends FieldT[Int]
  object stringField1 //extends FieldT[String]
  object stringField2 //extends FieldT[String]
  object boolField1 //extends FieldT[Boolean]
  object boolField2 //extends FieldT[Boolean]
  object doubleField1 //extends FieldT[Double]
  object doubleField2 //extends FieldT[Double]

  @Test
  def testSingletons {
    val r = ("foo" ->> 23) :: ("bar" ->> true) :: ("baz" ->> 2.0) :: HNil

    val v1 = r("foo")
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r("bar")
    typed[Boolean](v2)
    assertEquals(true, v2)

    val v3 = r("baz")
    typed[Double](v3)
    assertEquals(2.0, v3, Double.MinPositiveValue)

    val i1 = r.at(0)
    typed[Int](i1)
    assertEquals(23, v1)

    val i2 = r.at(1)
    typed[Boolean](i2)
    assertEquals(true, v2)

    val i3 = r.at(2)
    typed[Double](i3)
    assertEquals(2.0, v3, Double.MinPositiveValue)
  }

  @Test
  def testGet {
    import SingletonTypes._

    val r1 =
      (intField1    ->>    23) ::
      (stringField1 ->> "foo") ::
      (boolField1   ->>  true) ::
      (doubleField1 ->>   2.0) ::
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
    import SingletonTypes._

    val r1 =
      (intField1    ->>    23) ::
      (stringField1 ->> "foo") ::
      (boolField1   ->>  true) ::
      (doubleField1 ->>   2.0) ::
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
      (intField1    ->>    23) ::
      (stringField1 ->> "foo") ::
      (boolField1   ->>  true) ::
      (doubleField1 ->>   2.0) ::
      HNil

    val r2 = 
      (intField2    ->>    13) ::
      (stringField2 ->> "bar") ::
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
      (intField1    ->>    23) ::
      (stringField1 ->> "foo") ::
      HNil
      
    val r2 = r1 + (boolField1 ->> true)
    typed[FieldType[intField1.type, Int] :: FieldType[stringField1.type, String] :: FieldType[boolField1.type, Boolean] :: HNil](r2)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (boolField1 ->> true) :: HNil, r2)
    
    val r3 = r2 + (doubleField1 ->> 2.0)
    typed[FieldType[intField1.type, Int] :: FieldType[stringField1.type, String] :: FieldType[boolField1.type, Boolean] :: FieldType[doubleField1.type, Double] :: HNil](r3)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, r3)
  }
  
  @Test
  def testRemove {
    val r1 =
      (intField1    ->>    23) ::
      (stringField1 ->> "foo") ::
      (boolField1   ->>  true) ::
      (doubleField1 ->>   2.0) ::
      HNil
      
    val rm1 = r1.remove(intField1)
    typed[(Int, FieldType[stringField1.type, String] :: FieldType[boolField1.type, Boolean] :: FieldType[doubleField1.type, Double] :: HNil)](rm1)
    assertEquals(23, rm1._1)
    assertEquals((stringField1 ->> "foo") :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, rm1._2)
    
    val rm2 = r1.remove(stringField1)
    typed[(String, FieldType[intField1.type, Int] :: FieldType[boolField1.type, Boolean] :: FieldType[doubleField1.type, Double] :: HNil)](rm2)
    assertEquals("foo", rm2._1)
    assertEquals((intField1 ->> 23) :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, rm2._2)
    
    val rm3 = r1.remove(boolField1)
    typed[(Boolean, FieldType[intField1.type, Int] :: FieldType[stringField1.type, String] :: FieldType[doubleField1.type, Double] :: HNil)](rm3)
    assertEquals(true, rm3._1)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (doubleField1 ->> 2.0) :: HNil, rm3._2)

    val rm4 = r1.remove(doubleField1)
    typed[(Double, FieldType[intField1.type, Int] :: FieldType[stringField1.type, String] :: FieldType[boolField1.type, Boolean] :: HNil)](rm4)
    assertEquals(2.0, rm4._1, Double.MinPositiveValue)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (boolField1 ->> true) :: HNil, rm4._2)
    
    val r2 = r1 - intField1
    typed[FieldType[stringField1.type, String] :: FieldType[boolField1.type, Boolean] :: FieldType[doubleField1.type, Double] :: HNil](r2)
    assertEquals((stringField1 ->> "foo") :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, r2)

    val r3 = r1 - stringField1
    typed[FieldType[intField1.type, Int] :: FieldType[boolField1.type, Boolean] :: FieldType[doubleField1.type, Double] :: HNil](r3)
    assertEquals((intField1 ->> 23) :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, r3)
    
    val r4 = r1 - boolField1
    typed[FieldType[intField1.type, Int] :: FieldType[stringField1.type, String] :: FieldType[doubleField1.type, Double] :: HNil](r4)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (doubleField1 ->> 2.0) :: HNil, r4)
    
    val r5 = r1 - doubleField1
    typed[FieldType[intField1.type, Int] :: FieldType[stringField1.type, String] :: FieldType[boolField1.type, Boolean] :: HNil](r5)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (boolField1 ->> true) :: HNil, r5)
  }
}
