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
  import record._
  import syntax.singleton._

  def typed[T](t : => T) {}

  object intField1 extends FieldOf[Int]
  object intField2 extends FieldOf[Int]
  object stringField1 extends FieldOf[String]
  object stringField2 extends FieldOf[String]
  object boolField1 extends FieldOf[Boolean]
  object boolField2 extends FieldOf[Boolean]
  object doubleField1 extends FieldOf[Double]
  object doubleField2 extends FieldOf[Double]

  @Test
  def testGet {
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
  def testGetLiterals {
    val r1 =
      ("intField1"    ->>    23) ::
      ("stringField1" ->> "foo") ::
      ("boolField1"   ->>  true) ::
      ("doubleField1" ->>   2.0) ::
      HNil
    
    val v1 = r1.get("intField1")
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1.get("stringField1")
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r1.get("boolField1")
    typed[Boolean](v3)
    assertEquals(true, v3)
    
    val v4 = r1.get("doubleField1")
    typed[Double](v4)
    assertEquals(2.0, v4, Double.MinPositiveValue)
  }

  @Test
  def testAt {
    val r1 =
      (intField1    ->>    23) ::
      (stringField1 ->> "foo") ::
      (boolField1   ->>  true) ::
      (doubleField1 ->>   2.0) ::
      HNil
    
    val v1 = r1.at(0)
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1.at(1)
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r1.at(2)
    typed[Boolean](v3)
    assertEquals(true, v3)
    
    val v4 = r1.at(3)
    typed[Double](v4)
    assertEquals(2.0, v4, Double.MinPositiveValue)
  }

  @Test
  def testAtLiterals {
    val r1 =
      ("intField1"    ->>    23) ::
      ("stringField1" ->> "foo") ::
      ("boolField1"   ->>  true) ::
      ("doubleField1" ->>   2.0) ::
      HNil
    
    val v1 = r1.at(0)
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1.at(1)
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r1.at(2)
    typed[Boolean](v3)
    assertEquals(true, v3)
    
    val v4 = r1.at(3)
    typed[Double](v4)
    assertEquals(2.0, v4, Double.MinPositiveValue)
  }

  @Test
  def testUpdate {
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
  def testUpdateLiteral {
    val r1 =
      ("intField1"    ->>    23) ::
      ("stringField1" ->> "foo") ::
      ("boolField1"   ->>  true) ::
      ("doubleField1" ->>   2.0) ::
      HNil

    val r2 = r1.updated("intField1", 7)
    val v1 = r2.get("intField1")
    typed[Int](v1)
    assertEquals(7, v1)
    
    val r3 = r1.updated("stringField1", "wibble")
    val v2 = r3.get("stringField1")
    typed[String](v2)
    assertEquals("wibble", v2)
    
    val r4 = r1.updated("boolField1", false)
    val v3 = r4.get("boolField1")
    typed[Boolean](v3)
    assertEquals(false, v3)
    
    val r5 = r1.updated("doubleField1", 1.0)
    val v4 = r5.get("doubleField1")
    typed[Double](v4)
    assertEquals(1.0, v4, Double.MinPositiveValue)
    
    val r6 = HNil

    val r7 = r6.updated("boolField2", false)
    val v5 = r7.get("boolField2")
    typed[Boolean](v5)
    assertEquals(false, v5)
    
    val r8 = r7.updated("doubleField2", 3.0)
    val v6 = r8.get("doubleField2")
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
  def testConcatenateLiteral {
    val r1 =
      ("intField1"    ->>    23) ::
      ("stringField1" ->> "foo") ::
      ("boolField1"   ->>  true) ::
      ("doubleField1" ->>   2.0) ::
      HNil

    val r2 = 
      ("intField2"    ->>    13) ::
      ("stringField2" ->> "bar") ::
      r1

    val v1 = r2.get("intField2")
    typed[Int](v1)
    assertEquals(13, v1)
    
    val v2 = r2.get("stringField2")
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
  
  val wIntField1 = Witness("intField1")
  val wStringField1 = Witness("stringField1")
  val wBoolField1 = Witness("boolField1")
  val wDoubleField1 = Witness("doubleField1")

  @Test
  def testAppendLiteral {
    val r1 =
      ("intField1"    ->>    23) ::
      ("stringField1" ->> "foo") ::
      HNil
      
    val r2 = r1 + ("boolField1" ->> true)
    typed[FieldType[wIntField1.T, Int] :: FieldType[wStringField1.T, String] :: FieldType[wBoolField1.T, Boolean] :: HNil](r2)
    assertEquals(("intField1" ->> 23) :: ("stringField1" ->> "foo") :: ("boolField1" ->> true) :: HNil, r2)
    
    val r3 = r2 + ("doubleField1" ->> 2.0)
    typed[FieldType[wIntField1.T, Int] :: FieldType[wStringField1.T, String] :: FieldType[wBoolField1.T, Boolean] :: FieldType[wDoubleField1.T, Double] :: HNil](r3)
    assertEquals(("intField1" ->> 23) :: ("stringField1" ->> "foo") :: ("boolField1" ->> true) :: ("doubleField1" ->> 2.0) :: HNil, r3)
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

  @Test
  def testRemoveLiteral {
    val r1 =
      ("intField1"    ->>    23) ::
      ("stringField1" ->> "foo") ::
      ("boolField1"   ->>  true) ::
      ("doubleField1" ->>   2.0) ::
      HNil
      
    val rm1 = r1.remove("intField1")
    typed[(Int, FieldType[wStringField1.T, String] :: FieldType[wBoolField1.T, Boolean] :: FieldType[wDoubleField1.T, Double] :: HNil)](rm1)
    assertEquals(23, rm1._1)
    assertEquals(("stringField1" ->> "foo") :: ("boolField1" ->> true) :: ("doubleField1" ->> 2.0) :: HNil, rm1._2)
    
    val rm2 = r1.remove("stringField1")
    typed[(String, FieldType[wIntField1.T, Int] :: FieldType[wBoolField1.T, Boolean] :: FieldType[wDoubleField1.T, Double] :: HNil)](rm2)
    assertEquals("foo", rm2._1)
    assertEquals(("intField1" ->> 23) :: ("boolField1" ->> true) :: ("doubleField1" ->> 2.0) :: HNil, rm2._2)
    
    val rm3 = r1.remove("boolField1")
    typed[(Boolean, FieldType[wIntField1.T, Int] :: FieldType[wStringField1.T, String] :: FieldType[wDoubleField1.T, Double] :: HNil)](rm3)
    assertEquals(true, rm3._1)
    assertEquals(("intField1" ->> 23) :: ("stringField1" ->> "foo") :: ("doubleField1" ->> 2.0) :: HNil, rm3._2)

    val rm4 = r1.remove("doubleField1")
    typed[(Double, FieldType[wIntField1.T, Int] :: FieldType[wStringField1.T, String] :: FieldType[wBoolField1.T, Boolean] :: HNil)](rm4)
    assertEquals(2.0, rm4._1, Double.MinPositiveValue)
    assertEquals(("intField1" ->> 23) :: ("stringField1" ->> "foo") :: ("boolField1" ->> true) :: HNil, rm4._2)
    
    val r2 = r1 - "intField1"
    typed[FieldType[wStringField1.T, String] :: FieldType[wBoolField1.T, Boolean] :: FieldType[wDoubleField1.T, Double] :: HNil](r2)
    assertEquals(("stringField1" ->> "foo") :: ("boolField1" ->> true) :: ("doubleField1" ->> 2.0) :: HNil, r2)

    val r3 = r1 - "stringField1"
    typed[FieldType[wIntField1.T, Int] :: FieldType[wBoolField1.T, Boolean] :: FieldType[wDoubleField1.T, Double] :: HNil](r3)
    assertEquals(("intField1" ->> 23) :: ("boolField1" ->> true) :: ("doubleField1" ->> 2.0) :: HNil, r3)
    
    val r4 = r1 - "boolField1"
    typed[FieldType[wIntField1.T, Int] :: FieldType[wStringField1.T, String] :: FieldType[wDoubleField1.T, Double] :: HNil](r4)
    assertEquals(("intField1" ->> 23) :: ("stringField1" ->> "foo") :: ("doubleField1" ->> 2.0) :: HNil, r4)
    
    val r5 = r1 - "doubleField1"
    typed[FieldType[wIntField1.T, Int] :: FieldType[wStringField1.T, String] :: FieldType[wBoolField1.T, Boolean] :: HNil](r5)
    assertEquals(("intField1" ->> 23) :: ("stringField1" ->> "foo") :: ("boolField1" ->> true) :: HNil, r5)
  }

  @Test
  def testMappingOverRecordFields {
    object toUpper extends Poly1 {
      implicit def stringToUpper[F] = at[FieldType[F, String]] {
        f => field[F](f.toUpperCase)
      }

      implicit def otherTypes[X] = at[X](identity)
    }

    val r = ("foo" ->> "joe") :: ("bar" ->> true) :: ("baz" ->> 2.0) :: HNil
    val r2 = r map toUpper

    val v1 = r2("foo")
    typed[String](v1)
    assertEquals("JOE", v1)

    val v2 = r2("bar")
    typed[Boolean](v2)
    assertEquals(true, v2)

    val v3 = r2("baz")
    typed[Double](v3)
    assertEquals(2.0, v3, Double.MinPositiveValue)
  }

  @Test
  def testUpdateFieldByFunction {
    val r = ("foo" ->> 23) :: ("bar" ->> true) :: ("baz" ->> 2.0) :: HNil
    val r2 = r.updateWith("foo")((i: Int) => i.toString)
    val r2b = r.updateWith("foo")(i => i.toString)
    val r2c = r.updateWith("foo")(_.toString)

    val v21 = r2c.get("foo")
    typed[String](v21)
    assertEquals("23", v21)

    val v22 = r2c("bar")
    typed[Boolean](v22)
    assertEquals(true, v22)

    val v23 = r2c("baz")
    typed[Double](v23)
    assertEquals(2.0, v23, Double.MinPositiveValue)

    val r3 = r.updateWith("foo")((i: Int) => i+1)
    val r3b = r.updateWith("foo")(i => i+1)
    val r3c = r.updateWith("foo")(_ + 1)

    val v31 = r3c.get("foo")
    typed[Int](v31)
    assertEquals(24, v31)

    val v32 = r3c("bar")
    typed[Boolean](v32)
    assertEquals(true, v32)

    val v33 = r3c("baz")
    typed[Double](v33)
    assertEquals(2.0, v33, Double.MinPositiveValue)
  }

  @Test
  def testWidening {
    val ps = List(
      ("name"  ->> "Mel")  ::
      ("age"   ->> 90L)    ::
      ("teeth" ->> 2)      :: HNil,
      
      ("name"  ->> "Jude") ::
      ("age"   ->> 99L)    ::
      ("teeth" ->> 3)      :: HNil,
      
      ("name"  ->> "Bif")  ::
      ("age"   ->> 1L)     ::
      ("teeth" ->> 1)      :: HNil
    )
    
    ps.sortBy(_("age"))
    ps.sortBy(_("teeth"))
  }
}
