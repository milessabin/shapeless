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

class RecordTests {
  import labelled._
  import record._
  import syntax.singleton._
  import syntax.std.maps._
  import test._
  import testutil._
  import ops.record.RemoveAll

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
  def testFieldAt {
    val r1 =
      (intField1    ->>  "toto") ::
      (boolField1   ->>  true)   ::
      HNil

    val v1 = r1.fieldAt(intField1)
    val v2 = r1.fieldAt(boolField1)
    typed[FieldType[intField1.type, String]](v1)
    typed[FieldType[boolField1.type, Boolean]](v2)
    assertEquals("toto", v1)
    assertEquals(true, v2)
    assertEquals(r1, v1 :: v2 :: HNil)
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
  def testFromMap {
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
  def testFromMap2 {
    import test._

    type T = FieldType[intField1.type, Int] :: FieldType[stringField1.type, String] :: FieldType[boolField1.type, Boolean] :: FieldType[doubleField1.type, Double] :: HNil


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
  def testMerge {
    val r1 = 'a ->> 23 :: 'b ->> "foo" :: 'c ->> true :: HNil
    val r2 = 'c ->> false :: 'a ->> 13 :: HNil
    val rExp = 'a ->> 13 :: 'b ->> "foo" :: 'c ->> false :: HNil

    val rm = r1.merge(r2)
    typed[Record.`'a -> Int, 'b -> String, 'c -> Boolean`.T](rm)
    assertEquals(rExp, rm)
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
  def testRemoveAll {
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

  @Test
  def testRenameField {
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
  def testFieldPoly {
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
  def testFieldPolyOnRecord {
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
  def testFieldPolyNested {
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
  def testSelectDynamic {
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
  def testRecordTypeSelector {
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
  def testNamedArgs {
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
  def testNamedArgsInject {
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
  def testRecordArgs {
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

  @Test
  def testFields {
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
  def testToMap {
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
  def testMapValues {
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
}
