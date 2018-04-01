/*
 * Copyright (c) 2011-14 Miles Sabin
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
import shapeless.ops.record.AlignByKeys

class RecordTests {
  import labelled._
  import ops.record.LacksKey
  import record._
  import syntax.singleton._
  import syntax.std.maps._
  import test._
  import testutil._

  // making it method local causes weird compile error in Scala 2.10
  import ops.record.{ RemoveAll, UnzipFields }

  object intField1 extends FieldOf[Int]
  object intField2 extends FieldOf[Int]
  object stringField1 extends FieldOf[String]
  object stringField2 extends FieldOf[String]
  object boolField1 extends FieldOf[Boolean]
  object boolField2 extends FieldOf[Boolean]
  object doubleField1 extends FieldOf[Double]
  object doubleField2 extends FieldOf[Double]

  case class Bar(a: Int, b: String)

  @Test
  def testGet: Unit = {
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
  def testGetLiterals: Unit = {
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
  def testFieldAt: Unit = {
    val r1 =
      (stringField1 ->>  "toto") ::
      (boolField1   ->>  true)   ::
      HNil

    val v1 = r1.fieldAt(stringField1)
    val v2 = r1.fieldAt(boolField1)
    typed[stringField1.F](v1)
    typed[boolField1.F](v2)
    assertEquals("toto", v1)
    assertEquals(true, v2)
    assertEquals(r1, v1 :: v2 :: HNil)
  }

  @Test
  def testAt: Unit = {
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
  def testFromMap: Unit = {
    type T1 = Record.`'stringVal -> String, 'intVal -> Int, 'boolVal -> Boolean`.T

    val in = Map('intVal -> 4, 'stringVal -> "Blarr", 'boolVal -> true)


    val recOption = in.toRecord[T1]

    assert(recOption.isDefined)

    val rec: T1 = recOption.get

    typed[T1](rec)

    assert(rec('stringVal) == "Blarr", "stringVal mismatch")
    assert(rec('intVal) == 4, "int val mismatch")
    assert(rec('boolVal), "Boolean val match")

    val in2 = Map('intVal -> 4, 'stringVal -> "Blarr")

    val recEither2 = in2.toRecord[T1]

    assert(recEither2.isEmpty)
  }

  @Test
  def testFromMap2: Unit = {
    import test._

    type T = intField1.F :: stringField1.F :: boolField1.F :: doubleField1.F :: HNil


    val in = Map(intField1 -> 4, stringField1 -> "Blarr", boolField1 -> true, doubleField1 -> 5.0)

    import syntax.std.maps._

    val recOption = in.toRecord[T]

    assert(recOption.isDefined)

    val rec: T = recOption.get

    typed[T](rec)

    assert(rec(intField1) == 4)
    assert(rec(stringField1) == "Blarr")
    assert(rec(doubleField1) == 5.0)
  }


  @Test
  def testAtLiterals: Unit = {
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
  def testUpdate: Unit = {
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
  def testUpdateLiteral: Unit = {
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
  def testMerge: Unit = {
    val r1 = 'a ->> 23 :: 'b ->> "foo" :: 'c ->> true :: HNil
    val r2 = 'c ->> false :: 'a ->> 13 :: HNil
    val rExp = 'a ->> 13 :: 'b ->> "foo" :: 'c ->> false :: HNil

    val rm = r1.merge(r2)
    typed[Record.`'a -> Int, 'b -> String, 'c -> Boolean`.T](rm)
    assertEquals(rExp, rm)
  }

  @Test
  def testDeepMerge: Unit = {

    val r3 = Record(d = Record(x = "X1", m = "M"), e = true, x = "X")
    val r4 = Record(d = "D", e = false, x = 2, m = 6)
    val r5 = Record(d = "A", d = "B", d  = "C")


    assertTypedEquals(r4.merge(r3))(r4.deepMerge(r3))
    assertTypedEquals(r3.merge(r4))(r3.deepMerge(r4))
    assertTypedEquals(r3.merge(r5))(r3.deepMerge(r5))
    assertTypedEquals(r5.merge(r3))(r5.deepMerge(r3))

    //nested
    val inner1 = Record(d = "D", e = false)
    val inner2 = Record(d = 3, m = 2D)
    val outer1 = Record(d = 10, e = inner1, x = "boo")
    val outer2 = Record(x = "foo", d = -1, e = inner2)

    val innerMerged12 = inner1.merge(inner2)
    val innerMerged21 = inner2.merge(inner1)

    assertTypedEquals(Record(d = -1, e = innerMerged12,  x = "foo"))(outer1.deepMerge(outer2))
    assertTypedEquals(Record(x = "boo", d = 10, e = innerMerged21))(outer2.deepMerge(outer1))

    //complete intersection
    val inner11 = Record(d = "D2", e = true)
    val outer11 = Record(d = 11, e = inner11, x = "bar")
    assertTypedEquals(outer11)(outer1.deepMerge(outer11))
    assertTypedEquals(outer1)(outer11.deepMerge(outer1))

    //retain type of subrecord if it appears as first parameter
    val inner12 = Record(e = true, d = "D12",  x = 5)
    test.sameTyped(inner12)(inner12.deepMerge(inner1))

  }

  @Test
  def testExtract: Unit = {

    val inner1 = Record(d = 3, m = 2D, x= "X")
    val outer1 = Record(x = "foo", d = -1, e = inner1)

    type i = Record.`'x -> String, 'd -> Int`.T
    type i1 = Record.`'x -> Any, 'd -> Any`.T
    val extRes = Record(e = Record(x = "X", d = 3), d = -1)
    assertTypedEquals(extRes)(outer1.extract[Record.`'e -> i, 'd -> Int`.T])
    //covariance
    assertEquals(extRes, outer1.extract[Record.`'e -> i1, 'd -> Any`.T])

    type ill1 = Record.`'d -> Int, 'z -> Int`.T
    type ill2 = Record.`'x -> i`.T
    type illIner = Record.`'m -> String, 'd -> Int`.T
    type ill3 = Record.`'e -> illIner, 'd -> Int`.T


    illTyped("outer1.extract[ill1]")
    illTyped("outer1.deepExtract[ill2]")
    illTyped("outer1.deepExtract[ill3]")
  }

  @Test
  def testMergeWith: Unit = {
    object mergeField extends Poly2 {
      implicit def xor = at[Boolean, Boolean] { _ ^ _ }
      implicit def toDouble = at[Int, String] { _.toDouble + _.toDouble }
    }

    {
      val r1 = 'c ->> true :: HNil
      val r2 = 'c ->> false :: HNil
      val rExp = 'c ->> true :: HNil

      val rm = r1.mergeWith(r2)(mergeField)
      typed[Record.`'c -> Boolean`.T](rm)
      assertEquals(rExp, rm)
    }

    {
      val r1 = 'a ->> 23 :: 'b ->> "foo" :: 'c ->> true :: HNil
      val r2 = 'c ->> false :: 'a ->> "13" :: HNil
      val rExp = 'a ->> 36.0 :: 'b ->> "foo" :: 'c ->> true :: HNil

      val rm = r1.mergeWith(r2)(mergeField)
      typed[Record.`'a -> Double, 'b -> String, 'c -> Boolean`.T](rm)
      assertEquals(rExp, rm)
    }
  }

  @Test
  def testConcatenate: Unit = {
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
  def testConcatenateLiteral: Unit = {
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
  def testAppend: Unit = {
    val r1 =
      (intField1    ->>    23) ::
      (stringField1 ->> "foo") ::
      HNil

    val r2 = r1 + (boolField1 ->> true)
    typed[intField1.F :: stringField1.F :: boolField1.F :: HNil](r2)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (boolField1 ->> true) :: HNil, r2)

    val r3 = r2 + (doubleField1 ->> 2.0)
    typed[intField1.F :: stringField1.F :: boolField1.F :: doubleField1.F :: HNil](r3)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, r3)
  }

  val wIntField1 = Witness("intField1")
  val wStringField1 = Witness("stringField1")
  val wBoolField1 = Witness("boolField1")
  val wDoubleField1 = Witness("doubleField1")

  @Test
  def testAppendLiteral: Unit = {
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
  def testRemove: Unit = {
    val r1 =
      (intField1    ->>    23) ::
      (stringField1 ->> "foo") ::
      (boolField1   ->>  true) ::
      (doubleField1 ->>   2.0) ::
      HNil

    val rm1 = r1.remove(intField1)
    typed[(Int, stringField1.F :: boolField1.F :: doubleField1.F :: HNil)](rm1)
    assertEquals(23, rm1._1)
    assertEquals((stringField1 ->> "foo") :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, rm1._2)

    val rm2 = r1.remove(stringField1)
    typed[(String, intField1.F :: boolField1.F :: doubleField1.F :: HNil)](rm2)
    assertEquals("foo", rm2._1)
    assertEquals((intField1 ->> 23) :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, rm2._2)

    val rm3 = r1.remove(boolField1)
    typed[(Boolean, intField1.F :: stringField1.F :: doubleField1.F :: HNil)](rm3)
    assertEquals(true, rm3._1)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (doubleField1 ->> 2.0) :: HNil, rm3._2)

    val rm4 = r1.remove(doubleField1)
    typed[(Double, intField1.F :: stringField1.F :: boolField1.F :: HNil)](rm4)
    assertEquals(2.0, rm4._1, Double.MinPositiveValue)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (boolField1 ->> true) :: HNil, rm4._2)

    val r2 = r1 - intField1
    typed[stringField1.F :: boolField1.F :: doubleField1.F :: HNil](r2)
    assertEquals((stringField1 ->> "foo") :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, r2)

    val r3 = r1 - stringField1
    typed[intField1.F :: boolField1.F :: doubleField1.F :: HNil](r3)
    assertEquals((intField1 ->> 23) :: (boolField1 ->> true) :: (doubleField1 ->> 2.0) :: HNil, r3)

    val r4 = r1 - boolField1
    typed[intField1.F :: stringField1.F :: doubleField1.F :: HNil](r4)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (doubleField1 ->> 2.0) :: HNil, r4)

    val r5 = r1 - doubleField1
    typed[intField1.F :: stringField1.F :: boolField1.F :: HNil](r5)
    assertEquals((intField1 ->> 23) :: (stringField1 ->> "foo") :: (boolField1 ->> true) :: HNil, r5)
  }

  @Test
  def testRemoveLiteral: Unit = {
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
  def testReplace: Unit = {
    type R = Record.`'a -> Int, 'b -> String`.T
    val a = Record(a = 1, b = "2")
    val r = a.replace('a, 2)

    typed[R](r)
    assertEquals(Record(a = 2, b = "2"), r)

    illTyped(""" a.replace('a, ()) """)
  }

  @Test
  def testLacksKey: Unit = {
    def without[R <: HList, O <: HList](k: Witness)(r: R)(f: R => O)(implicit ev: LacksKey[R, k.T]): O = f(r)

    type R1 = Record.`'a -> Int, 'b -> String, 'c -> Boolean`.T
    type R2 = Record.`'c -> Boolean, 'a -> Int, 'b -> String`.T

    val a = Record(a = 1, b = "2")

    val r1 = without('c)(a)(_ :+ ('c ->> true))
    typed[R1](r1)
    assertEquals(Record(a = 1, b = "2", c = true), r1)

    val r2 = without('c)(a)(('c ->> true) +: _)
    typed[R2](r2)
    assertEquals(Record(c = true, a = 1, b = "2"), r2)

    illTyped(""" without('a)(a)(identity) """)
  }

  @Test
  def testRemoveAll: Unit = {

    type R = Record.`'i -> Int, 's -> String, 'c -> Char, 'j -> Int`.T
    type L = Record.`'c -> Char, 'j -> Int`.T

    type A1 = Record.`'i -> Int, 's -> String`.T
    type A2 = Int :: String :: HNil

    val r = 'i ->> 10 :: 's ->> "foo" :: 'c ->> 'x' :: 'j ->> 42 :: HNil

    val removeAll1 = RemoveAll[R, A1]
    val removeAll2 = RemoveAll[R, A2]

    val (removed1, remaining1) = removeAll1(r)
    val (removed2, remaining2) = removeAll2(r)

    val r1 = removeAll1.reinsert((removed1, remaining1))
    val r2 = removeAll2.reinsert((removed2, remaining2))

    typed[A1](removed1)
    assertEquals('i ->> 10 :: 's ->> "foo" :: HNil, removed1)

    typed[A2](removed2)
    assertEquals(10 :: "foo" :: HNil, removed2)

    typed[L](remaining1)
    assertEquals('c ->> 'x' :: 'j ->> 42 :: HNil, remaining1)

    typed[L](remaining2)
    assertEquals('c ->> 'x' :: 'j ->> 42 :: HNil, remaining2)

    typed[R](r1)
    assertEquals(r, r1)

    typed[R](r2)
    assertEquals(r, r2)
  }

  @Test
  def testMappingOverRecordFields: Unit = {
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
  def testUpdateFieldByFunction: Unit = {
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
  def testWidening: Unit = {
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

  @Test
  def testRenameField: Unit = {
    val r = ("foo" ->> 23) :: ("bar" ->> true) :: HNil
    val r1 = r.renameField("foo", "foobar")

    val v1 = r1.get("foobar")
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r1.get("bar")
    typed[Boolean](v2)
    assertEquals(true, v2)
  }

  @Test
  def testFieldPoly: Unit = {
    import poly._

    object f extends FieldPoly {
      implicit def atFoo = atField[Int]("foo")(_ + 1)
    }

    val r = "foo" ->> 23

    val r1 = f(r)

    val fooType = "foo".witness

    typed[FieldType[fooType.T, Int]](r1)
    assertEquals(24, r1)
  }


  @Test
  def testFieldPolyOnRecord: Unit = {
    import poly._

    object f extends FieldPoly {
      implicit def atFoo = atField[Int]("foo")(_ + 1)
    }

    val r = ("foo" ->> 23) :: ("bar" ->> true) :: HNil

    val r1 = everywhere(f)(r)

    val v1 = r1("foo")
    typed[Int](v1)
    assertEquals(24, v1)

    val v2 = r1("bar")
    typed[Boolean](v2)
    assertEquals(true, v2)
  }

  @Test
  def testFieldPolyNested: Unit = {
    import poly._

    object f extends FieldPoly {
      implicit def atFoo = atField[Int]("foo")(_ + 1)
    }

    val r = List(List(List(("foo" ->> 23) :: ("bar" ->> true) :: HNil)))

    val List(List(List(r1))) = everywhere(f)(r)

    val v1 = r1("foo")
    typed[Int](v1)
    assertEquals(24, v1)

    val v2 = r1("bar")
    typed[Boolean](v2)
    assertEquals(true, v2)
  }

  @Test
  def testSelectDynamic: Unit = {
    val r = ('foo ->> 23) :: ('bar ->> true) :: HNil
    val d = r.record

    val v1 = d.foo
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = d.bar
    typed[Boolean](v2)
    assertEquals(true, v2)

    illTyped("d.baz")
  }

  @Test
  def testRecordTypeSelector: Unit = {
    typed[Record.` `.T](HNil)

    typed[Record.`'i -> Int`.T]('i ->> 23 :: HNil)

    typed[Record.`'i -> Int, 's -> String`.T]('i ->> 23 :: 's ->> "foo" :: HNil)

    typed[Record.`'i -> Int, 's -> String, 'b -> Boolean`.T]('i ->> 23 :: 's ->> "foo" :: 'b ->> true :: HNil)

    // Literal types

    typed[Record.`'i -> 2`.T]('i ->> 2.narrow :: HNil)

    typed[Record.`'i -> 2, 's -> "a", 'b -> true`.T]('i ->> 2.narrow :: 's ->> "a".narrow :: 'b ->> true.narrow :: HNil)

    illTyped(""" typed[Record.`'i -> 2`.T]('i ->> 3.narrow :: HNil) """)

    // Mix of standard and literal types

    typed[Record.`'i -> 2, 's -> String, 'b -> true`.T]('i ->> 2.narrow :: 's ->> "a" :: 'b ->> true.narrow :: HNil)
  }

  @Test
  def testNamedArgs: Unit = {
    {
      val r = Record()
      typed[HNil](r)
    }

    {
      val r = Record(i = 23, s = "foo", b = true)
      typed[Record.`'i -> Int, 's -> String, 'b -> Boolean`.T](r)
    }

    {
      illTyped(""" Record(2, "a") """)
    }
  }

  @Test
  def testNamedArgsInject: Unit = {
    val r = Record(i = 23, s = "foo", b = true)

    val v1 = r.get('i)
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r.get('s)
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r.get('b)
    typed[Boolean](v3)
    assertEquals(true, v3)

    illTyped("""
      r.get('foo)
    """)
  }

  object Foo extends RecordArgs {
    def applyRecord[R <: HList](rec: R): R = rec
  }

  @Test
  def testRecordArgs: Unit = {
    val r = Foo(i = 23, s = "foo", b = true)
    typed[Record.`'i -> Int, 's -> String, 'b -> Boolean`.T](r)

    val v1 = r.get('i)
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = r.get('s)
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = r.get('b)
    typed[Boolean](v3)
    assertEquals(true, v3)

    illTyped("""
      r.get('foo)
    """)
  }

  object Bar extends FromRecordArgs {
    def sum(i1: Int, i2: Int) = i1 + i2
    def sumImplicit(i1: Int)(implicit i2: Int) = i1 + i2
    def sumMultipleParamList(i1: Int)(i2: Int) = i1 + i2
  }

  @Test
  def testFromRecordArgs: Unit = {
    val r = ('i1 ->> 1) :: ('i2 ->> 3) :: HNil

    val v1 = Bar.sumRecord(r)
    typed[Int](v1)
    assertEquals(4, v1)

    val r2 = r.merge(('i2 ->> 2) :: HNil)
    val v2 = Bar.sumMultipleParamListRecord(r2)
    typed[Int](v2)
    assertEquals(3, v2)

    illTyped("""
      Bar.sumImplicitRecord(('i1 ->> 1) :: ('i2 ->> 3) :: HNil)
    """)

    implicit val i2 = 7
    val v3 = Bar.sumImplicitRecord(r)
    typed[Int](v2)
    assertEquals(8, v3)

    illTyped("""
      Bar.sumRecord(('i1 ->> 1) :: ('i3 ->> 3) :: HNil)
    """)

    illTyped("""
      Bar.sumMultipleParamListRecord(('i1 ->> 1) :: ('i3 ->> 3) :: HNil)
    """)
  }

  @Test
  def testFields: Unit = {
    {
      val f = HNil.fields
      assertTypedEquals(HNil, f)
    }

    {
      val f = (HNil: HNil).fields
      assertTypedEquals(HNil: HNil, f)
    }

    val r = Record(i = 23, s = "foo", b = true)

    {
      val f = r.fields
      assertTypedEquals(('i.narrow -> 23) :: ('s.narrow -> "foo") :: ('b.narrow -> true) :: HNil, f)
    }

    val rs = ("first" ->> Some(2)) :: ("second" ->> Some(true)) :: ("third" ->> Option.empty[String]) :: HNil

    {
      val f = rs.fields
      assertTypedEquals(("first".narrow -> Some(2)) :: ("second".narrow -> Some(true)) :: ("third" -> Option.empty[String]) :: HNil, f)
    }
  }

  @Test
  def testUnzipFields: Unit = {
    {
      val uf = UnzipFields[HNil]
      assertTypedEquals(HNil, uf.keys)
      assertTypedEquals(HNil, uf.values(HNil))
    }

    {
      val uf = UnzipFields[HNil]
      assertTypedEquals(HNil: HNil, uf.keys)
      assertTypedEquals(HNil: HNil, uf.values(HNil: HNil))
    }

    type R = Record.`'i -> Int, 's -> String, 'b -> Boolean`.T
    val r: R = Record(i = 23, s = "foo", b = true)

    {
      val uf = UnzipFields[R]
      assertTypedEquals('i.narrow :: 's.narrow :: 'b.narrow :: HNil, uf.keys)
      assertTypedEquals(23 :: "foo" :: true :: HNil, uf.values(r))
    }

    type RS = Record.`"first" -> Option[Int], "second" -> Option[Boolean], "third" -> Option[String]`.T
    val rs: RS = ("first" ->> Some(2)) :: ("second" ->> Some(true)) :: ("third" ->> Option.empty[String]) :: HNil

    {
      val uf = UnzipFields[RS]
      assertTypedEquals("first".narrow :: "second".narrow :: "third" :: HNil, uf.keys)
      assertTypedEquals(Some(2) :: Some(true) :: Option.empty[String] :: HNil, uf.values(rs))
    }
  }

  @Test
  def testToMap: Unit = {
    {
      val m = HNil.toMap
      assertTypedEquals(Map.empty[Any, Nothing], m)
    }

    {
      val m = HNil.toMap[String, Nothing]
      assertTypedEquals(Map.empty[String, Nothing], m)
    }

    {
      val m = HNil.toMap[String, Int]
      assertTypedEquals(Map.empty[String, Int], m)
    }

    val r = Record(i = 23, s = "foo", b = true)

    {
      val m = r.toMap
      assertTypedEquals(Map[Symbol, Any]('i -> 23, 's -> "foo", 'b -> true), m)
    }

    {
      val m = r.toMap[Symbol, Any]
      assertTypedEquals(Map[Symbol, Any]('i -> 23, 's -> "foo", 'b -> true), m)
    }

    val rs = ("first" ->> Some(2)) :: ("second" ->> Some(true)) :: ("third" ->> Option.empty[String]) :: HNil

    {
      val m = rs.toMap
      assertTypedEquals(Map[String, Option[Any]]("first" -> Some(2), "second" -> Some(true), "third" -> Option.empty[String]), m)
    }

    {
      val m = rs.toMap[String, Option[Any]]
      assertTypedEquals(Map[String, Option[Any]]("first" -> Some(2), "second" -> Some(true), "third" -> Option.empty[String]), m)
    }
  }

  @Test
  def testMapValues: Unit = {
    object f extends Poly1 {
      implicit def int = at[Int](i => i > 0)
      implicit def string = at[String](s => s"s: $s")
      implicit def boolean = at[Boolean](v => if (v) "Yup" else "Nope")
    }

    {
      val r = HNil
      val res = r.mapValues(f)
      assertTypedEquals[HNil](HNil, res)
    }

    {
      val r = Record(i = 23, s = "foo", b = true)
      val res = r.mapValues(f)
      assertTypedEquals[Record.`'i -> Boolean, 's -> String, 'b -> String`.T](Record(i = true, s = "s: foo", b = "Yup"), res)
    }

    {
      object toUpper extends Poly1 {
        implicit def stringToUpper = at[String](_.toUpperCase)
        implicit def otherTypes[X] = at[X](identity)
      }

      val r = ("foo" ->> "joe") :: ("bar" ->> true) :: ("baz" ->> 2.0) :: HNil
      val r2 = r mapValues toUpper

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
  }

  @Test
  def testSwapRecord: Unit = {
    import shapeless.ops.record.SwapRecord

    val rt = Record.`'x -> Int, 'y -> String, 'z -> Boolean`
    type TestRecord = rt.T

    val (x, y, z) = (Witness('x), Witness('y), Witness('z))

    val fields: (FieldType[Int, x.T] :: FieldType[String, y.T] :: FieldType[Boolean, z.T] :: HNil) = SwapRecord[TestRecord].apply

    assertEquals(fields.toList, List('x, 'y, 'z))
  }

  @Test
  def alignByKeys: Unit = {
    type TestRecord = Record.`'a -> String, 'b -> Int, 'c -> Double`.T

    type Keys1 = HList.`'a, 'b, 'c`.T
    type Keys2 = HList.`'b, 'c, 'a`.T
    type Keys3 = HList.`'b, 'a, 'c`.T
    type Keys4 = HList.`'c, 'a, 'b`.T

    val v = Record(a  = "foo", b  = 42, c = 33.3)

    assertTypedEquals[TestRecord](v, AlignByKeys[TestRecord, Keys1].apply(v))
    assertTypedEquals[Record.`'b -> Int, 'c -> Double, 'a -> String`.T](Record(b = 42, c = 33.3, a = "foo"), AlignByKeys[TestRecord, Keys2].apply(v))

    assertTypedEquals[Record.`'b -> Int, 'a -> String, 'c -> Double`.T](Record(b = 42, a = "foo", c = 33.3), v.alignByKeys[Keys3])
    assertTypedEquals[Record.`'c -> Double, 'a -> String, 'b -> Int`.T](Record(c = 33.3, a = "foo", b = 42), v.alignByKeys[Keys4])
  }

  @Test
  def testSelectorWithTaggedType: Unit = {
    import tag.@@

    val tagged = tag[Int]("42")
    val head1 = 'k ->> tagged
    val head2 = field[Witness.`'k`.T](tagged)
    val rec1 = head1 :: HNil
    val rec2 = head2 :: HNil

    assertTypedEquals[String @@ Int](rec1('k), rec2('k))
  }

  @Test
  def testSelectorWithTaggedType2: Unit = {
    import tag.@@

    trait TestTag
    case class FooT(bar: String @@ TestTag)
    val lgt = LabelledGeneric[FooT]
    val fooT = FooT(tag[TestTag]("test"))

    assertEquals(tag[TestTag]("test"), lgt.to(fooT).get('bar))
  }

  @Test
  def testSelectorForSwappedRecord: Unit = {
    import ops.record.{ Selector, SwapRecord }

    val gen = LabelledGeneric[Bar]
    val swap = SwapRecord[gen.Repr]
    val select = Selector[swap.Out, Int]
    val swapped = swap()

    assertTypedEquals[Witness.`'a`.T](swapped.head, select(swapped))
 }
}
