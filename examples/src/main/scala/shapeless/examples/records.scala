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
  import HList._
  import Record._

  object author  extends Field[String]  { override def toString = "Author" }
  object title   extends Field[String]  { override def toString = "Title" }
  object id      extends Field[Int]     { override def toString = "ID" }
  object price   extends Field[Double]  { override def toString = "Price" }
  object inPrint extends Field[Boolean] { override def toString = "In print" }

  def printBook[B <: HList](b : B)(implicit tl : ToList[B, (Field[_], Any)]) = {
    b.toList foreach { case (field, value) => println(field+": "+value) }
    println
  }
    
  val book =
    (author -> "Benjamin Pierce") ::
    (title  -> "Types and Programming Languages") ::
    (id     ->  262162091) ::
    (price  ->  44.11) ::
    HNil
  
  printBook(book)
    
  // Read price field
  val currentPrice = book.get(price)  // Static type is Double
  println("Current price is "+currentPrice)
  println

  // Update price field, relying on static type of currentPrice
  val updated = book + (price -> (currentPrice+2.0))
  printBook(updated)

  // Add a new field
  val extended = updated + (inPrint -> true)
  printBook(extended)
  
  // Remove a field
  val noId = extended - id 
  printBook(noId)
}