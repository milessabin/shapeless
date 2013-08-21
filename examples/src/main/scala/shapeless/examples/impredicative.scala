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

// See http://research.microsoft.com/en-us/um/people/simonpj/papers/boxy/boxy-icfp.pdf
object ImpredicativeExamples extends App {
  import shapeless._
  import poly._
  
  def typed[T](t : => T) {}
  
  object head extends (List ~> Id) {
    def apply[T](l : List[T]) = l.head
  }
  
  def g(o : Option[List ~> Id]) = o match {
    case None => (0, '0')
    case Some(get) => (get(List(1, 2)), get(List('a', 'b', 'c')))
  }
  
  val gNone = g(None)
  typed[(Int, Char)](gNone)
  assert(gNone == (0, '0'))
  
  val gSome = g(Option(head))
  typed[(Int, Char)](gSome)
  assert(gSome == (1, 'a'))
}
