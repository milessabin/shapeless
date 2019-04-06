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

package shapeless.examples

import shapeless._

/**
 * LabelledGeneric examples.
 * 
 * @author Miles Sabin
 */
object LabelledGenericExamples extends App {
  import record._
  import ops.record._
  import syntax.singleton._

  case class Book(author: String, title: String, id: Int, price: Double)
  case class ExtendedBook(author: String, title: String, id: Int, price: Double, inPrint: Boolean)

  val bookGen = LabelledGeneric[Book]
  val bookExtGen = LabelledGeneric[ExtendedBook]

  val tapl = Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11)

  val rec = bookGen.to(tapl)

  // Read price field
  val currentPrice = rec(Symbol("price"))  // Static type is Double
  println("Current price is "+currentPrice)
  println

  // Update price field, relying on static type of currentPrice
  val updated = bookGen.from(rec.updateWith(Symbol("price"))(_+2.0))
  println(updated)
  println

  // Add a new field, map back into ExtendedBook
  val extended = bookExtGen.from(rec + (Symbol("inPrint") ->> true)) // Static type is ExtendedBook
  println(extended)
  println

  // internationalization Shapeless style?
  case class Libro(autor: String, `título`: String, id: Int, precio: Double)

  val libroGen = LabelledGeneric[Libro]
  val libroKeys = Keys[libroGen.Repr]
  val libroRec = rec.values.zipWithKeys(libroKeys())
  val libro = libroGen.from(libroRec) // static type is Libro
  println(libro)
  println
}

/**
 * Utility trait intended for inferring a field type from a sample value and unpacking it into its
 * key and value types.
 */
import labelled.FieldType

trait Field {
  type K
  type V
  type F = FieldType[K, V]
}

object Field {
  def apply[K0, V0](sample: FieldType[K0, V0]) = new Field { type K = K0; type V = V0 }
}

object OldWineNewBottles extends App {
  import ops.hlist.Align
  import syntax.singleton._

  case class From(s1: String, s2: String)
  case class To(s2: String, i: Int, s1: String)

  val from = From("foo", "bar")

  val fromGen = LabelledGeneric[From]
  val toGen = LabelledGeneric[To]

  // Define the type of the i field by example
  val iField = Field(Symbol("i") ->> 0)

  val align = Align[iField.F :: fromGen.Repr, toGen.Repr]

  val to = toGen.from(align(Symbol("i") ->> 23 :: fromGen.to(from)))
  println(to)
  println
}
