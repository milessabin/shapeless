/*
 * Copyright (c) 2013-15 Miles Sabin
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

import ops.{ hlist => hl, coproduct => cp }
import testutil.assertTypedEquals
import test.illTyped

package GenericTestsAux {

  sealed trait AbstractNonCC
  class NonCCA(val i: Int, val s: String) extends AbstractNonCC
  class NonCCB(val b: Boolean, val d: Double) extends AbstractNonCC
  class NonCCWithVars(var c: Char, var l: Long) extends AbstractNonCC
  class NonCCWithVal(val n: Int) extends AbstractNonCC {
    val isEven: Boolean = n % 2 == 0
  }

  class NonCCWithCompanion private (val i: Int, val s: String)
  object NonCCWithCompanion {
    def apply(i: Int, s: String) = new NonCCWithCompanion(i, s)
    def unapply(s: NonCCWithCompanion): Option[(Int, String)] = Some((s.i, s.s))
  }

  class NonCCLazy(prev0: => NonCCLazy, next0: => NonCCLazy) {
    lazy val prev = prev0
    lazy val next = next0
  }

  sealed trait Overlapping
  sealed trait OA extends Overlapping
  case class OAC(s: String) extends OA
  sealed trait OB extends Overlapping
  case class OBC(s: String) extends OB
  case class OAB(i: Int) extends OA with OB

  class CCLikeOrdered[A: Ordering](val value: A)

  class CCLikeDegen(val i: Int)()

  class Squared(x: Long) {
    val x2 = x * x
  }
}
object GenericTestsAuxScalaCompat {
  import GenericTestsAux._
  type TapRepr[A] = ConstTap[A] :+: InTap[A, _] :+: OutTap[A, _] :+: PipeTap[A, _] :+: CNil
}

class GenericScala2Tests {
  import GenericTestsAux._
  import scala.collection.immutable.{ :: => Cons }
  import test._

  @Test
  def testAbstractNonCC: Unit = {
    val ncca = new NonCCA(23, "foo")
    val nccb = new NonCCB(true, 2.0)
    val nccc = new NonCCWithVars('c', 42)
    val nccd = new NonCCWithVal(313)
    val ancc: AbstractNonCC = ncca

    val genA = Generic[NonCCA]
    val genB = Generic[NonCCB]
    val genC = Generic[NonCCWithVars]
    val genD = Generic[NonCCWithVal]
    val genAbs = Generic[AbstractNonCC]

    val rA = genA.to(ncca)
    assertTypedEquals[Int :: String :: HNil](23 :: "foo" :: HNil, rA)

    val rB = genB.to(nccb)
    assertTypedEquals[Boolean :: Double :: HNil](true :: 2.0 :: HNil, rB)

    val rC = genC.to(nccc)
    assertTypedEquals[Char :: Long :: HNil]('c' :: 42L :: HNil, rC)

    val rD = genD.to(nccd)
    assertTypedEquals[Int :: HNil](313 :: HNil, rD)

    val rAbs = genAbs.to(ancc)
    assertTypedEquals[NonCCA :+: NonCCB :+: NonCCWithVars :+: NonCCWithVal :+: CNil](Inl(ncca), rAbs)

    val fA = genA.from(13 :: "bar" :: HNil)
    typed[NonCCA](fA)
    assertEquals(13, fA.i)
    assertEquals("bar", fA.s)

    val fB = genB.from(false :: 3.0 :: HNil)
    typed[NonCCB](fB)
    assertEquals(false, fB.b)
    assertEquals(3.0, fB.d, Double.MinPositiveValue)

    val fC = genC.from('k' :: 313L :: HNil)
    typed[NonCCWithVars](fC)
    assertEquals('k', fC.c)
    assertEquals(313L, fC.l)

    val fD = genD.from(99 :: HNil)
    typed[NonCCWithVal](fD)
    assertEquals(99, fD.n)

    val fAbs = genAbs.from(Inr(Inl(nccb)))
    typed[AbstractNonCC](fAbs)
    assertTrue(fAbs.isInstanceOf[NonCCB])
    assertEquals(true, fAbs.asInstanceOf[NonCCB].b)
    assertEquals(2.0, fAbs.asInstanceOf[NonCCB].d, Double.MinPositiveValue)
  }

  @Test
  def testNonCCWithCompanion: Unit = {
    val nccc = NonCCWithCompanion(23, "foo")

    val gen = Generic[NonCCWithCompanion]

    val r = gen.to(nccc)
    assertTypedEquals[Int :: String :: HNil](23 :: "foo" :: HNil, r)

    val f = gen.from(13 :: "bar" :: HNil)
    typed[NonCCWithCompanion](f)
    assertEquals(13, f.i)
    assertEquals("bar", f.s)
  }

  @Test
  def testNonCCLazy: Unit = {
    lazy val (a: NonCCLazy, b: NonCCLazy, c: NonCCLazy) =
      (new NonCCLazy(c, b), new NonCCLazy(a, c), new NonCCLazy(b, a))

    val gen = Generic[NonCCLazy]

    val rB = gen.to(b)
    assertTypedEquals[NonCCLazy :: NonCCLazy :: HNil](a :: c :: HNil, rB)

    val fD = gen.from(a :: c :: HNil)
    typed[NonCCLazy](fD)
    assertEquals(a, fD.prev)
    assertEquals(c, fD.next)
  }

  @Test
  def testOverlappingCoproducts: Unit = {
    val gen = Generic[Overlapping]
    val o: Overlapping = OAB(1)
    val o0 = gen.to(o)
    typed[OAC :+: OAB :+: OBC :+: CNil](o0)

    val o1 = gen.from(o0)
    typed[Overlapping](o1)
  }

  trait Parent {
    case class Nested(i: Int, s: String)

    sealed abstract class Foo extends Product with Serializable

    case object A extends Foo
    case object B extends Foo
    case class C() extends Foo
  }

  trait Child extends Parent {
    val gen = Generic[Nested]
    val adtGen = Generic[Foo]
  }

  object O extends Child

  @Test
  def testNestedInherited: Unit = {
    val n0 = O.Nested(23, "foo")
    val repr = O.gen.to(n0)
    typed[Int :: String :: HNil](repr)
    val n1 = O.gen.from(repr)
    typed[O.Nested](n1)
    assertEquals(n0, n1)

    {
      val foo0 = O.B
      val repr = O.adtGen.to(foo0)
      typed[O.A.type :+: O.B.type :+: O.C :+: CNil](repr)
    }

    {
      val foo0 = O.C()
      val repr = O.adtGen.to(foo0)
      typed[O.A.type :+: O.B.type :+: O.C :+: CNil](repr)
    }
  }

  @Test
  def testPathViaSubPrefix: Unit = {
    class Outer {
      class Inner {
        sealed trait T
      }

      val inner = new Inner
      case class C(i: Int) extends inner.T
      case object O extends inner.T
    }

    val outer = new Outer
    type Repr = outer.C :+: outer.O.type :+: CNil
    val gen = Generic[outer.inner.T]
    val c = outer.C(42)
    val injC: Repr = Inl(c)
    val injO: Repr = Inr(Inl(outer.O))

    assertTypedEquals[Repr](injC, gen.to(c))
    assertTypedEquals[outer.inner.T](c, gen.from(injC))
    assertTypedEquals[Repr](injO, gen.to(outer.O))
    assertTypedEquals[outer.inner.T](outer.O, gen.from(injO))
  }

  @Test
  def testGenericImplicitParams: Unit = {
    type Repr = Int :: HNil
    val gen = Generic[CCOrdered[Int]]
    val cc = CCOrdered(42)
    val rep = 42 :: HNil

    assertTypedEquals[CCOrdered[Int]](gen.from(rep), cc)
    assertTypedEquals[Repr](gen.to(cc), rep)
    illTyped("Generic[CCLikeOrdered[Int]]")
  }

  @Test
  def testGenericDegenerate: Unit = {
    type Repr = Int :: HNil
    val gen = Generic[CCDegen]
    val cc = CCDegen(313)()
    val rep = 313 :: HNil

    assertTypedEquals[CCDegen](gen.from(rep), cc)
    assertTypedEquals[Repr](gen.to(cc), rep)
    illTyped("Generic[CCLikeDegen]")
  }
}

object MixedCCNonCCNested {
  // Block local
  {
    object T1{
      sealed abstract class Tree
      final case class Node(left: Tree, right: Tree, v: Int) extends Tree
      case object Leaf extends Tree
    }

    Generic[T1.Tree]
    import T1._
    Generic[Tree]

    sealed trait A
    sealed case class B(i: Int, s: String) extends A
    case object C extends A
    sealed trait D extends A
    final case class E(a: Double, b: Option[Float]) extends D
    case object F extends D
    sealed abstract class Foo extends D
    case object Baz extends Foo
    final class Bar extends Foo
    final class Baz(val i1: Int, val s1: String) extends Foo

    Generic[A]
    Generic[B]
    Generic[C.type]
    Generic[D]
    Generic[E]
    Generic[F.type]
    Generic[Foo]
    Generic[Baz.type]
    Generic[Bar]
    Generic[Baz]
  }

  def methodLocal: Unit = {
    object T1{
      sealed abstract class Tree
      final case class Node(left: Tree, right: Tree, v: Int) extends Tree
      case object Leaf extends Tree
    }

    Generic[T1.Tree]
    import T1._
    Generic[Tree]

    sealed trait A
    sealed case class B(i: Int, s: String) extends A
    case object C extends A
    sealed trait D extends A
    final case class E(a: Double, b: Option[Float]) extends D
    case object F extends D
    sealed abstract class Foo extends D
    case object Baz extends Foo
    final class Bar extends Foo
    final class Baz(val i1: Int, val s1: String) extends Foo

    Generic[A]
    Generic[B]
    Generic[C.type]
    Generic[D]
    Generic[E]
    Generic[F.type]
    Generic[Foo]
    Generic[Baz.type]
    Generic[Bar]
    Generic[Baz]
  }

  // Top level
  object T1{
    sealed abstract class Tree
    final case class Node(left: Tree, right: Tree, v: Int) extends Tree
    case object Leaf extends Tree
  }

  Generic[T1.Tree]
  import T1._
  Generic[Tree]

  sealed trait A
  sealed case class B(i: Int, s: String) extends A
  case object C extends A
  sealed trait D extends A
  final case class E(a: Double, b: Option[Float]) extends D
  case object F extends D
  sealed abstract class Foo extends D
  case object Baz extends Foo
  final class Bar extends Foo
  final class Baz(val i1: Int, val s1: String) extends Foo

  Generic[A]
  Generic[B]
  Generic[C.type]
  Generic[D]
  Generic[E]
  Generic[F.type]
  Generic[Foo]
  Generic[Baz.type]
  Generic[Bar]
  Generic[Baz]
}

object EnumDefns0 {
  sealed trait EnumVal
  val BarA = new EnumVal { val name = "A" }
  val BarB = new EnumVal { val name = "B" }
  val BarC = new EnumVal { val name = "C" }
}

object EnumDefns3 {
  sealed trait EnumVal
  val BarA, BarB, BarC = new EnumVal {}
}

object EnumDefns4 {
  sealed trait EnumVal
  object EnumVal {
    val BarA = new EnumVal { val name = "A" }
    val BarB = new EnumVal { val name = "B" }
    val BarC = new EnumVal { val name = "C" }
  }
}

object EnumDefns7 {
  sealed trait EnumVal
  object EnumVal {
    val BarA, BarB, BarC = new EnumVal {}
  }
}

class TestEnumScala2 {
  @Test
  def testEnum0: Unit = {
    import EnumDefns0._

    val gen = Generic[EnumVal]
    val a0 = gen.to(BarA)
    assert(a0 == Inl(BarA))

    val b0 = gen.to(BarB)
    assert(b0 == Inr(Inl(BarB)))

    val c0 = gen.to(BarC)
    assert(c0 == Inr(Inr(Inl(BarC))))
  }

  @Test
  def testEnum3: Unit = {
    import EnumDefns3._

    val gen = Generic[EnumVal]
    val a0 = gen.to(BarA)
    assert(a0 == Inl(BarA))

    val b0 = gen.to(BarB)
    assert(b0 == Inr(Inl(BarB)))

    val c0 = gen.to(BarC)
    assert(c0 == Inr(Inr(Inl(BarC))))
  }

  @Test
  def testEnum4: Unit = {
    import EnumDefns4._
    import EnumVal._

    val gen = Generic[EnumVal]
    val a0 = gen.to(BarA)
    assert(a0 == Inl(BarA))

    val b0 = gen.to(BarB)
    assert(b0 == Inr(Inl(BarB)))

    val c0 = gen.to(BarC)
    assert(c0 == Inr(Inr(Inl(BarC))))
  }

  @Test
  def testEnum7: Unit = {
    import EnumDefns7._
    import EnumVal._

    val gen = Generic[EnumVal]
    val a0 = gen.to(BarA)
    assert(a0 == Inl(BarA))

    val b0 = gen.to(BarB)
    assert(b0 == Inr(Inl(BarB)))

    val c0 = gen.to(BarC)
    assert(c0 == Inr(Inr(Inl(BarC))))
  }
}

package TestPrefixes1 {

  object DerivationsScala2 {
    import shapeless._

    //TODO: Should work. Stopped working as we stopped sorting the types
    //Generic[Defs.Sum]
    //Generic.materialize[Defs.Sum, Defs.SumI :+: Defs.SumS :+: CNil]
  }
}

object PathVariantDefns {
  sealed trait AtomBase {
    sealed trait Atom
    case class Zero(value: String) extends Atom
  }

  trait Atom1 extends AtomBase {
    case class One(value: String) extends Atom
  }

  trait Atom2 extends AtomBase {
    case class Two(value: String) extends Atom
  }

  object Atoms01 extends AtomBase with Atom1
  object Atoms02 extends AtomBase with Atom2
}

object PathVariants {
  import PathVariantDefns._

  val gen1 = Generic[Atoms01.Atom]
  implicitly[gen1.Repr =:= (Atoms01.Zero :+: Atoms01.One :+: CNil)]

  val gen2 = Generic[Atoms02.Atom]
  implicitly[gen2.Repr =:= (Atoms02.Zero :+: Atoms02.Two :+: CNil)]
}

object Thrift {
  object TProduct {
    def apply(a: Double, b: String): TProduct = new Immutable(a, b)

    def unapply(tp: TProduct): Option[Product2[Double, String]] = Some(tp)

    //class Immutable(val a: Double, val b: String) extends TProduct

    class Immutable(
                     val a: Double,
                     val b: String,
                     val _passthroughFields: scala.collection.immutable.Map[Short, Byte]
                   ) extends TProduct {
      def this(
                a: Double,
                b: String
              ) = this(
        a,
        b,
        Map.empty
      )
    }
  }

  trait TProduct extends Product2[Double, String] {
    def a: Double
    def b: String

    def _1 = a
    def _2 = b

    override def productPrefix: String = "TProduct"

    def canEqual(t: Any): Boolean = true
  }

  Generic[TProduct.Immutable]
}
