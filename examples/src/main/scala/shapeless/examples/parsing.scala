/*
 * Copyright (c) 2012 Miles Sabin 
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

import scala.util.parsing.combinator._

/**
 * An illustration of shapeless techniques applied to Scala's combinator parsers.
 * See http://stackoverflow.com/questions/9594392 for context.
 */
object CombinatorTesting extends App {
  
  object CombinatorParser extends RegexParsers {
    lazy val a = "a"
    lazy val b = "b"
    lazy val c = "c"
    lazy val content = a ~ b ~ c // ^^ {case a~b => a::b::c::Nil work but I want something more general that work for any ~ length.
  }

  import CombinatorParser._

  /**
   * Type class supporting the flattening of a `ParseResult` over arbitrarily nested sequences of
   * `String` matches to a `List` of `String`s.
   */
  trait Flatten[M] extends (M => List[String]) {
    def apply(m : M) : List[String]
  }
  
  /**
   * Type class instance for `String`.
   */
  implicit def flattenString = new Flatten[String] {
    def apply(m : String) = List(m) 
  }
  
  /**
   * Flatten instance for `A ~ B`. Requires Flatten instances for `A` and `B`. 
   */
  implicit def flattenPattern[A, B](implicit flattenA : Flatten[A], flattenB : Flatten[B]) = new Flatten[A ~ B] {
    def apply(m : A ~ B) = m match { case a ~ b => flattenA(a) ::: flattenB(b) } 
  }
  
  /**
   * Flatten instance for ParseResult[T]. Requires a Flatten instance for T.
   */
  implicit def flattenParseResult[T](implicit flattenP : Flatten[T]) = new Flatten[ParseResult[T]] {
    def apply(p : ParseResult[T]) = (p map flattenP) getOrElse Nil 
  }
  
  def flatten[P](p : P)(implicit flatten : Flatten[P]) = flatten(p)
  
  val testChar = "abc"
  val output = parseAll(content, testChar)
  println(output)          // ((a~b)~c) but I want List(a, b, c)
    
  val flattenedOutput = flatten(output)
  println(flattenedOutput) // List(a, b, c)
}
