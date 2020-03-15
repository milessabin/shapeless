/*
 * Copyright (c) 2011-13 Miles Sabin 
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
 * Sized collection examples.
 * 
 * @author Miles Sabin
 */
object SizedExamples extends App {
  import shapeless._
  import syntax.sized._

  def sequence[T](lo : List[Option[T]]) = if (lo.exists(_.isEmpty)) None else Some(lo.map(_.get))
  
  def row(cols : Seq[String]) = cols.mkString("\"", "\", \"", "\"")
  
  def csv[N <: Nat](hdrs : Sized[Seq[String], N], rows : List[Sized[Seq[String], N]]) =
    row(hdrs) :: rows.map(row(_))
  
  def fullyStatic: Unit = {
    val hdrs = Sized("Title", "Author")                     // Sized[IndexedSeq[String], _2]
    val rows = List(                                        // List[Sized[IndexedSeq[String], _2]]
      Sized("Types and Programming Languages", "Benjamin Pierce"),
      Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones")
    )
  
    // hdrs and rows statically known to have the same number of columns
    val formatted = csv(hdrs, rows)
    formatted foreach println                               // Compiles
    
    println
    
    // extendedHdrs has the wrong number of columns for rows
    val extendedHdrs = Sized("Title", "Author", "ISBN")     // Sized[IndexedSeq[Int], _3]
    //val badFormatted = csv(threeHdrs, rows)               // Does not compile

    // Extend the rows to match ...
    val extendedRows = rows map (_ :+ "-")                  // List[Sized[IndexedSeq[String], _3]]
    
    val extendedFormatted = csv(extendedHdrs, extendedRows) // Compiles
    extendedFormatted foreach println
  }

  def mixedDynamicStatic: Unit = {
    val hdrs = List("Title", "Author")
    val rows = List(
      List("Types and Programming Languages", "Benjamin Pierce"),
      List("The Implementation of Functional Programming Languages", "Simon Peyton-Jones")
    )
    
    for {
      shdrs <- hdrs.sized(2)
      srows <- sequence(rows map (_.sized(2)))
    } {
      // If we get here then our lists are statically know to be
      // of the appropriate sizes
      val formatted = csv(shdrs, srows)
      formatted foreach println
    }
    
    println
  
    // extendedHdrs has the wrong number of columns for rows
    val extendedHdrs = List("Title", "Author", "ISBN")
  
    for {
      shdrs <- extendedHdrs.sized(2)   // This will be empty ... 
      srows <- sequence(rows map (_.sized(2)))
    } {
      // ... hence, not reached
      val formatted = csv(shdrs, srows)
      formatted foreach println
    }
    
    // Extend the rows to match ...
    val extendedRows = rows map (_ :+ "-")

    for {
      shdrs <- extendedHdrs.sized(3)
      srows <- sequence(extendedRows map (_.sized(3)))
    } {
      // ... reached this time
      val formatted = csv(shdrs, srows)
      formatted foreach println
    }
    
  }
  
  println("Fully static: ")
  fullyStatic
  
  println
  
  println("Mixed dynamic/static")
  mixedDynamicStatic
}
