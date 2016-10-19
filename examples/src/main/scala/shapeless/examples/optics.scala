/*
 * Copyright (c) 2012-14 Miles Sabin 
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

/*
 * Examples of optic (ie. lens/prism) usage.
 * 
 * @author Miles Sabin
 */
package opticDemoDatatypes {
  case class Address(street : String, city : String)
  case class Person(name : String, age : Int, address : Address)

  sealed trait Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  case class Leaf[T](value: T) extends Tree[T]

  case class Foo(i: Int, s: String)
  case class Bar(i: Int, b: Boolean)

  sealed trait Either[+A, +B]
  case class Left[A, B](left: A) extends Either[A, B]
  case class Right[A, B](right: B) extends Either[A, B]
}

object OpticExamples extends App {
  import opticDemoDatatypes._

  // 1. Basic nested case classes
  val mary = Person("Mary", 32, Address("Southover Street", "Brighton"))
  val ageLens = lens[Person].age
  val streetLens = lens[Person].address.street
  
  val age = ageLens.get(mary)
  assert(age == 32)

  val street = streetLens.get(mary)
  assert(street == "Southover Street")

  val mary2 = streetLens.set(mary)("Montpelier Road")
  val mary3 = ageLens.modify(mary2)(_+1)
  assert(mary3 == Person("Mary", 33, Address("Montpelier Road", "Brighton")))

  // 2. Sealed family (sum of products)
  val l: Either[Int, Boolean] = Left(23)
  val r: Either[Int, Boolean] = Right(false)

  val lExplicit = prism[Either[Int, Boolean]][Left[Int, Boolean]].left
  val rExplicit = prism[Either[Int, Boolean]][Right[Int, Boolean]].right

  val ol = lExplicit.get(l)
  assert(ol == Some(23))

  val or = rExplicit.get(l)
  assert(or == None)

  // 3. Sealed family with coproduct branch inferred from product selectors

  val lInferred = prism[Either[Int, Boolean]].left
  val rInferred = prism[Either[Int, Boolean]].right

  val ol2 = lInferred.get(l)
  assert(ol2 == Some(23))

  val or2 = rInferred.get(l)
  assert(or2 == None)

  // 4. Sealed, recursive family, coproduct branches inferred

  val t1 = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
  val t2 = Node(Leaf(4), Node(Leaf(5), Leaf(6)))

  val lr = prism[Tree[Int]].left.right.value
  val rr = prism[Tree[Int]].right.right.value

  val lrv1 = lr.get(t1)
  assert(lrv1 == Some(2))

  val lrv2 = lr.get(t2)
  assert(lrv2 == None)

  val t1b = lr.set(t1)(23)
  assert(t1b == Node(Node(Leaf(1), Leaf(23)), Leaf(3)))

  val t2b = rr.set(t2)(13)
  assert(t2b == Node(Leaf(4), Node(Leaf(5), Leaf(13))))

  // 5. Optic inferred from initial data type and a path

  def update[T, E](t: T)(e: E)(implicit mkLens: p.Lens[T, E]): T = mkLens().set(t)(e)

  val p = ^.i // a path selecting the product element labelled 'i'

  // Unrelated types with a common field i: Int
  val foo = Foo(23, "foo")
  val bar = Bar(13, true)

  // Typesafe polymorphic update via lens inference
  val foo2 = update(foo)(11)
  assert(foo2 == Foo(11, "foo"))

  val bar2 = update(bar)(7)
  assert(bar2 == Bar(7, true))
}
