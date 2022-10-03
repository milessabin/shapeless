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

import org.junit.Test
import org.junit.Assert._

import test._

package ALaCarteTestsAux {
  object FooDefns extends DefaultCaseClassDefns {
    type C = Foo
    val ops = Ops
  }

  object Foo extends FooDefns.CaseClassCompanion
  class Foo(val i: Int, val s: String) extends FooDefns.CaseClass
}

class ALaCarteTests {
  import ALaCarteTestsAux._

  @Test
  def testApplyUnapply: Unit = {
    val foo = Foo(23, "foo")
    typed[Foo](foo)

    val Foo(i, s) = foo
    typed[Int](i)
    typed[String](s)
  }

  @Test
  def testProduct: Unit = {
    val foo = Foo(23, "foo")

    val foo_1 = foo.productElement(0)
    typed[Any](foo_1)
    assertEquals(23, foo_1)

    val foo_2 = foo.productElement(1)
    typed[Any](foo_2)
    assertEquals("foo", foo_2)

    val fooIterator = foo.productIterator
    assertEquals(List(23, "foo"), fooIterator.toList)

    val fooPrefix = foo.productPrefix
    assertEquals("Foo", fooPrefix)

    val fooArity = foo.productArity
    assertEquals(2, fooArity)
  }

  @Test
  def testPolyEquality: Unit = {
    val foo = Foo(23, "foo")
    val foo2 = Foo(23, "foo")
    val foo3 = Foo(13, "bar")
    assertEquals(foo, foo2)
    assertEquals(foo.hashCode, foo2.hashCode)
    assertFalse(foo == foo3)
  }

  @Test
  def testCopy: Unit = {
    val foo = Foo(23, "foo")
    val fooCopy = foo.copy()
    assertFalse(fooCopy eq foo)
    assertEquals(foo, fooCopy)
    assertEquals(foo.hashCode, fooCopy.hashCode)

    val mod = Foo(13, "foo")
    val fooMod = foo.copy(i = 13)
    assertFalse(fooMod eq foo)
    assertEquals(mod, fooMod)
    assertEquals(mod.hashCode, fooMod.hashCode)
  }

  @Test
  def testToString: Unit = {
    val foo = Foo(23, "foo")
    val fooStr = foo.toString
    assertEquals("Foo(23,foo)", fooStr)
  }
}
