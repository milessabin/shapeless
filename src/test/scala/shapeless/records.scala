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

  trait Field {
    type valueType
  }
  
  object intField1 extends Field { type valueType = Int }
  object intField2 extends Field { type valueType = Int }
  object stringField1 extends Field { type valueType = String }
  object stringField2 extends Field { type valueType = String }
  object boolField1 extends Field { type valueType = Boolean }
  object boolField2 extends Field { type valueType = Boolean }
  object doubleField1 extends Field { type valueType = Double }
  object doubleField2 extends Field { type valueType = Double }

  type FieldEntry[F <: Field] = (F, F#valueType)
  
  abstract class RecordOps[L <: HList](l : L) {
    def get[F <: Field](implicit find : Find[L, FieldEntry[F]]) : F#valueType = find(l)._2
    def get[F <: Field](f : F)(implicit find : Find[L, FieldEntry[F]]) : F#valueType = find(l)._2
  }
  
  implicit def recordOps[L <: HList](l : L) = new RecordOps(l) {} 
  
  trait Find[L <: HList, E] {
    def apply(l : L) : E
  }
  
  implicit def hlistFind1[H, T <: HList] = new Find[H :: T, H] {
    def apply(l : H :: T) : H = l.head
  }
  
  implicit def hlistFind2[H, T <: HList, E](implicit ft : Find[T, E]) = new Find[H :: T, E] {
    def apply(l : H :: T) : E = ft(l.tail)
  }
  
  @Test
  def testRecords {
    val record =
      (intField1    ->    23) ::
      (stringField1 -> "foo") ::
      (boolField1   ->  true) ::
      (doubleField1 ->   2.0) ::
      (intField2    ->    13) ::
      (stringField2 -> "bar") ::
      (boolField2   -> false) ::
      (doubleField2 ->   3.0) ::
      HNil
    
    val v1 = record.get(intField1)
    typed[Int](v1)
    assertEquals(23, v1)

    val v2 = record.get(stringField1)
    typed[String](v2)
    assertEquals("foo", v2)

    val v3 = record.get(boolField1)
    typed[Boolean](v3)
    assertEquals(true, v3)
    
    val v4 = record.get(doubleField1)
    typed[Double](v4)
    assertEquals(2.0, v4, Double.MinPositiveValue)

    val v5 = record.get(intField2)
    typed[Int](v5)
    assertEquals(13, v5)
    
    val v6 = record.get(stringField2)
    typed[String](v6)
    assertEquals("bar", v6)
    
    val v7 = record.get(boolField2)
    typed[Boolean](v7)
    assertEquals(false, v7)
    
    val v8 = record.get(doubleField2)
    typed[Double](v8)
    assertEquals(3.0, v8, Double.MinPositiveValue)
  }
}