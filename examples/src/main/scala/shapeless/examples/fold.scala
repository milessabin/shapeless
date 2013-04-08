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
 * Fully polymorphic left fold over an HList.
 *
 * @author Miles Sabin
 */
object FoldExamples extends App {
  import shapeless._

  // Polymorphic binary function value with type-specific cases:
  //  (c : Char, s : String) => s.indexOf(c)
  //  (i : Int, b : Boolean) => if ((i >= 0) == b) "pass" else "fail")
  object combine extends Poly {
    implicit def caseCharString = use((c : Char, s : String) => s.indexOf(c))
    implicit def caseIntBoolean = use((i : Int, b : Boolean) => if ((i >= 0) == b) "pass" else "fail")
  }

  // Computation is:
  // val c1a = "foo".indexOf('o')
  // val c1b = if ((c1a >= 0) == true) "pass" else "fail"
  val l1 = "foo" :: true :: HNil
  val f1 = l1.foldLeft('o')(combine)
  assert(f1 == "pass")

  // Computation is:
  // val c2a = "bar".indexOf('o')
  // val c2b = if ((c2a >= 0) == false) "pass" else "fail"
  val l2 = "bar" :: false :: HNil
  val f2 = l2.foldLeft('o')(combine)
  assert(f2 == "pass")  
}
