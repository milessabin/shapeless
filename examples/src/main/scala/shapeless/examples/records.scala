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

package shapeless.examples

/**
 * Record examples.
 * 
 * @author Miles Sabin
 */
object RecordExamples extends App {
  import shapeless._
  import record._
  import ops.hlist.ToList
  import ops.record.{ Keys, Values }
  import syntax.singleton._

  def printBook[B <: HList, K <: HList, V <: HList](b : B)
    (implicit
      keys: Keys.Aux[B, K],
      values: Values.Aux[B, V],
      ktl: ToList[K, Any],
      vtl: ToList[V, Any]) = {
    (b.keys.toList zip b.values.toList) foreach { case (field, value) => println(s"$field: $value") }
    println
  }

  val book =
    ("author" ->> "Benjamin Pierce") ::
    ("title"  ->> "Types and Programming Languages") ::
    ("id"     ->>  262162091) ::
    ("price"  ->>  44.11) ::
    HNil
  
  printBook(book)
    
  // Read price field
  val currentPrice = book("price")  // Static type is Double
  println("Current price is "+currentPrice)
  println

  // Update price field, relying on static type of currentPrice
  val updated = book + ("price" ->> (currentPrice+2.0))
  printBook(updated)

  // Add a new field
  val extended = updated + ("inPrint" ->> true)
  printBook(extended)
  
  // Remove a field
  val noId = extended - "id" 
  printBook(noId)
}
