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
package examples

import test._

// Almost boilerplate free case classes a la carte for any "case-class-like"
// type (in particular, "case-class-like" includes non-case-classes with
// lazy val fields).
//
// DefaultCaseClassDefns exactly replicates the default compiler supported semantics,
// and it is entirely possible to swap out polymorphic  equality for typesafe
// equality a la scalaz.Equal, alter (or remove) toString, remove the untyped
// productElements and related methods (or replace with typed alternatives).
//
// Note that no new macros were required for the creation of this library.

object ALaCarteDemo extends App {
  // Minimal boilerplate required for Foo to emulate a standard
  // Scala case class ...
  object FooDefns extends DefaultCaseClassDefns {
    type C = Foo
    val ops = Ops
  }

  // Definitions of Foo and its companion in terms of the prior
  // declaration ...
  object Foo extends FooDefns.CaseClassCompanion

  class Foo(val i: Int, val s: String) extends FooDefns.CaseClass

  // Our "case class" in use ...

  // Companion apply
  val foo = Foo(23, "foo")
  typed[Foo](foo)

  // Companion unapply
  val Foo(i, s) = foo
  typed[Int](i)
  typed[String](s)

  // product defns
  val foo_1 = foo.productElement(0)
  typed[Any](foo_1)
  assert(23 == foo_1)

  val foo_2 = foo.productElement(1)
  typed[Any](foo_2)
  assert("foo" == foo_2)

  val fooIterator = foo.productIterator
  assert(List(23, "foo") == fooIterator.toList)

  val fooPrefix = foo.productPrefix
  assert("Foo" == fooPrefix)

  val fooArity = foo.productArity
  assert(2 == fooArity)

  // polymorphic equality
  val foo2 = Foo(23, "foo")
  val foo3 = Foo(13, "bar")
  assert(foo == foo2)
  assert(foo.hashCode == foo2.hashCode)
  assert(foo != foo3)

  // copy
  val fooCopy = foo.copy()
  assert(fooCopy ne foo)
  assert(foo == fooCopy)
  assert(foo.hashCode == fooCopy.hashCode)

  val mod = Foo(13, "foo")
  val fooMod = foo.copy(i = 13)
  assert(fooMod ne foo)
  assert(mod == fooMod)
  assert(mod.hashCode == fooMod.hashCode)

  // toString
  val fooStr = foo.toString
  assert("Foo(23,foo)" == fooStr)
}
