/*
 * Copyright (c) 2014 Miles Sabin 
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

import ops.record._
import record._
import syntax.singleton._
import test._

object LabelledGenericTestsAux {
  case class Book(author: String, title: String, id: Int, price: Double)
  case class ExtendedBook(author: String, title: String, id: Int, price: Double, inPrint: Boolean)

  val tapl = Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11)
  val tapl2 = Book("Benjamin Pierce", "Types and Programming Languages (2nd Ed.)", 262162091, 46.11)
  val taplExt = ExtendedBook("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11, true)

  val taplRecord =
    ('author ->> "Benjamin Pierce") ::
    ('title  ->> "Types and Programming Languages") ::
    ('id     ->>  262162091) ::
    ('price  ->>  44.11) ::
    HNil

  val bookSchema = RecordType.like(taplRecord)
  type BookRec = bookSchema.Record
  type BookKeys = bookSchema.Keys
  type BookValues = bookSchema.Values
}

class LabelledGenericTests {
  import LabelledGenericTestsAux._

  @Test
  def testProductBasics {
    val gen = LabelledGeneric[Book]

    val b0 = gen.to(tapl)
    typed[BookRec](b0)
    assertEquals(taplRecord, b0)
    
    val b1 = gen.from(b0)
    typed[Book](b1)
    assertEquals(tapl, b1)

    val keys = b0.keys
    assertEquals('author.narrow :: 'title.narrow :: 'id.narrow :: 'price.narrow :: HNil, keys)

    val values = b0.values
    assertEquals("Benjamin Pierce" :: "Types and Programming Languages" :: 262162091 :: 44.11 :: HNil, values)
  }

  @Test
  def testGet {
    val gen = LabelledGeneric[Book]

    val b0 = gen.to(tapl)
    
    val e1 = b0.get('author)
    typed[String](e1)
    assertEquals("Benjamin Pierce", e1)

    val e2 = b0.get('title)
    typed[String](e2)
    assertEquals( "Types and Programming Languages", e2)

    val e3 = b0.get('id)
    typed[Int](e3)
    assertEquals(262162091, e3)

    val e4 = b0.get('price)
    typed[Double](e4)
    assertEquals(44.11, e4, Double.MinPositiveValue)
  }

  @Test
  def testApply {
    val gen = LabelledGeneric[Book]

    val b0 = gen.to(tapl)
    
    val e1 = b0('author)
    typed[String](e1)
    assertEquals("Benjamin Pierce", e1)

    val e2 = b0('title)
    typed[String](e2)
    assertEquals( "Types and Programming Languages", e2)

    val e3 = b0('id)
    typed[Int](e3)
    assertEquals(262162091, e3)

    val e4 = b0('price)
    typed[Double](e4)
    assertEquals(44.11, e4, Double.MinPositiveValue)
  }

  @Test
  def testAt {
    val gen = LabelledGeneric[Book]

    val b0 = gen.to(tapl)
    
    val v1 = b0.at(0)
    typed[String](v1)
    assertEquals("Benjamin Pierce", v1)

    val v2 = b0.at(1)
    typed[String](v2)
    assertEquals( "Types and Programming Languages", v2)

    val v3 = b0.at(2)
    typed[Int](v3)
    assertEquals(262162091, v3)
    
    val v4 = b0.at(3)
    typed[Double](v4)
    assertEquals(44.11, v4, Double.MinPositiveValue)
  }

  @Test
  def testUpdated {
    val gen = LabelledGeneric[Book]

    val b0 = gen.to(tapl)

    val b1 = b0.updated('title, "Types and Programming Languages (2nd Ed.)")
    val b2 = b1.updated('price, 46.11)

    val updated = gen.from(b2)
    assertEquals(tapl2, updated)
  }

  @Test
  def testUpdateWith {
    val gen = LabelledGeneric[Book]

    val b0 = gen.to(tapl)

    val b1 = b0.updateWith('title)(_+" (2nd Ed.)")
    val b2 = b1.updateWith('price)(_+2.0)

    val updated = gen.from(b2)
    assertEquals(tapl2, updated)
  }

  @Test
  def testExtension {
    val gen = LabelledGeneric[Book]
    val gen2 = LabelledGeneric[ExtendedBook]

    val b0 = gen.to(tapl)
    val b1 = b0 + ('inPrint ->> true)

    val b2 = gen2.from(b1)
    typed[ExtendedBook](b2)
    assertEquals(taplExt, b2)
  }
}
