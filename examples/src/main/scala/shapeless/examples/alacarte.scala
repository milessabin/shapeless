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

import scala.reflect.ClassTag

import record._
import ops.hlist.{ Length, Tupler }
import ops.nat.ToInt
import ops.record.Merger

trait CaseClassFacet {
  type C
}

trait ProductISOFacet extends CaseClassFacet {
  trait ProductISOOps {
    type Repr <: HList
    type P <: Product
    val gen: Generic.Aux[C, Repr]
    val pgen: Generic.Aux[P, Repr]

    def toProduct(c: C): P = pgen.from(gen.to(c))
    def fromProduct(p: P): C = gen.from(pgen.to(p))
  }

  val ops: ProductISOOps
}

trait ApplyUnapplyFacet extends ProductISOFacet {
  trait ApplyUnapplyOps extends ProductISOOps {
    def apply(p: P): C = fromProduct(p)

    def unapply(c: C): Some[P] = Some(toProduct(c))
  }

  val ops: ApplyUnapplyOps

  trait ApplyUnapplyCompanion {
    @nonGeneric def apply(elems: ops.P): C = ops.apply(elems)
    @nonGeneric def unapply(s: C): Some[ops.P] = ops.unapply(s)
  }
}

trait ProductFacet extends ProductISOFacet {
  trait ProductOps extends ProductISOOps {
    def productElement(c: C, n: Int): Any = toProduct(c).productElement(n)

    def productIterator(c: C): Iterator[Any] = toProduct(c).productIterator

    def productPrefix: String

    def productArity: Int
  }

  val ops: ProductOps

  trait ProductMethods { self: C =>
    def productElement(n: Int): Any = ops.productElement(this, n)

    def productIterator: Iterator[Any] = ops.productIterator(this)

    def productPrefix: String = ops.productPrefix

    def productArity: Int = ops.productArity
  }
}

trait PolymorphicEqualityFacet extends ProductISOFacet {
  trait PolymorphicEqualityOps extends ProductISOOps {
    val typ: Typeable[C]

    def canEqual(c: C, other: Any): Boolean = typ.cast(other).isDefined

    def equals(c: C, other: Any): Boolean =
      (c.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
        typ.cast(other).exists { that =>
          (toProduct(c) == toProduct(that)) && canEqual(that, c)
        }

    def hashCode(c: C): Int = toProduct(c).hashCode
  }

  val ops: PolymorphicEqualityOps

  trait PolymorphicEqualityMethods { self: C =>
    override def equals(other: Any): Boolean = ops.equals(this, other)

    override def hashCode: Int = ops.hashCode(this)
  }
}

trait ToStringFacet extends ProductFacet {
  trait ToStringOps extends ProductOps {
    def toString(c: C): String = productPrefix+toProduct(c).toString
  }

  val ops: ToStringOps

  trait ToStringMethods { self: C =>
    override def toString: String = ops.toString(this)
  }
}

trait DefaultCaseClassDefns
  extends ApplyUnapplyFacet
    with ProductFacet
    with PolymorphicEqualityFacet
    with ToStringFacet {

  trait CaseClassOps
    extends ApplyUnapplyOps
      with ProductOps
      with PolymorphicEqualityOps
      with ToStringOps

  trait CaseClassCompanion extends
    ApplyUnapplyCompanion

  trait CaseClass
    extends ProductMethods
      with PolymorphicEqualityMethods
      with ToStringMethods { self: C => }

  val ops: CaseClassOps

  def Ops[Repr0 <: HList, LRepr0 <: HList, P0 <: Product, N <: Nat]
  (implicit
   gen0: Generic.Aux[C, Repr0],
   lgen0: LabelledGeneric.Aux[C, LRepr0],
   len: Length.Aux[Repr0, N],
   toInt: ToInt[N],
   tup: Tupler.Aux[Repr0, P0],
   pgen0: Generic.Aux[P0, Repr0],
   typ0: Typeable[C],
   tag0: ClassTag[C]
  ) =
    new CaseClassOps {
      type Repr = Repr0
      type LRepr = LRepr0
      type P = P0
      val gen = gen0
      val lgen = lgen0
      val pgen = pgen0
      val typ = typ0
      val tag = tag0
      val productPrefix = tag0.runtimeClass.getName.split("(\\.|\\$)").last
      val productArity = toInt()
    }
}

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
