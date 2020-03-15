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

object KList {
  import shapeless._
  import UnaryTCConstraint._
  
  // Function which will only accept HList's whose elements all have Option as their
  // outer type constructor
  def acceptOption[L <: HList : *->*[Option]#λ](l : L) = true
  
  val l1 = Option(23) :: Option(true) :: Option("foo") :: HNil 
  val l2 = Option(23) :: true :: Option("foo") :: HNil
  
  acceptOption(l1)  // Compiles
  //acceptOption(l2)  // Does not compile
}
