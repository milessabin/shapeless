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
  sealed trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit
  case class Orange() extends Fruit

  object showFruit extends Poly1 {
    implicit def caseApple  = at[Apple](_ => "Pomme")
    implicit def casePear   = at[Pear](_ => "Poire")
    implicit def caseBanana = at[Banana](_ => "Banane")
    implicit def caseOrange = at[Orange](_ => "Orange")
  }

  sealed trait AbstractSingle
  case class Single() extends AbstractSingle

  sealed trait Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  case class Leaf[T](t: T) extends Tree[T]

  sealed trait Enum
  case object A extends Enum
  case object B extends Enum
  case object C extends Enum

  object pickFruit extends Poly1 {
    implicit def caseA = at[A.type](_ => Apple())
    implicit def caseB = at[B.type](_ => Banana())
    implicit def caseC = at[C.type](_ => Pear())
  }

  sealed trait L
  case object N extends L
  case class C(hd: Int, tl: L) extends L

  case class Company(depts : List[Dept])
  sealed trait Subunit
  case class Dept(name : String, manager : Employee, subunits : List[Subunit]) extends Subunit
  case class Employee(person : Person, salary : Salary) extends Subunit
  case class Person(name : String, address : String, age: Int)

  case class Salary(salary : Double)

  case class PersonWithPseudonims(name: String, nicks: String*)

  trait starLP extends Poly1 {
    implicit def default[T] = at[T](identity)
  }

  object star extends starLP {
    implicit def caseString = at[String](_+"*")

    implicit def caseIso[T, L <: HList](implicit gen: Generic.Aux[T, L], mapper: Lazy[hl.Mapper.Aux[this.type, L, L]]) =
      at[T](t => gen.from(mapper.value(gen.to(t))))
  }

  trait incLP extends Poly1 {
    implicit def default[T] = at[T](identity)
  }

  object inc extends incLP {
    implicit val caseInt = at[Int](_+1)

    implicit def caseProduct[T, L <: HList](implicit gen: Generic.Aux[T, L], mapper: hl.Mapper.Aux[this.type, L, L]) =
      at[T](t => gen.from(gen.to(t).map(inc)))

    implicit def caseCoproduct[T, L <: Coproduct](implicit gen: Generic.Aux[T, L], mapper: cp.Mapper.Aux[this.type, L, L]) =
      at[T](t => gen.from(gen.to(t).map(inc)))
  }

  sealed trait AbstractNonCC
  class NonCCA(val i: Int, val s: String) extends AbstractNonCC
  class NonCCB(val b: Boolean, val d: Double) extends AbstractNonCC

  class NonCCWithCompanion private (val i: Int, val s: String)
  object NonCCWithCompanion {
    def apply(i: Int, s: String) = new NonCCWithCompanion(i, s)
    def unapply(s: NonCCWithCompanion): Option[(Int, String)] = Some((s.i, s.s))
  }

  class NonCCLazy(prev0: => NonCCLazy, next0: => NonCCLazy) {
    lazy val prev = prev0
    lazy val next = next0
  }

  sealed trait Xor[+A, +B]
  case class Left[+LA](a: LA) extends Xor[LA, Nothing]
  case class Right[+RB](b: RB) extends Xor[Nothing, RB]

  sealed trait Base[BA, BB]
  case class Swap[SA, SB](a: SA, b: SB) extends Base[SB, SA]

  sealed trait Overlapping
  sealed trait OA extends Overlapping
  case class OAC(s: String) extends OA
  sealed trait OB extends Overlapping
  case class OBC(s: String) extends OB
  case class OAB(i: Int) extends OA with OB
}

class GenericTests {
  import GenericTestsAux._
  import scala.collection.immutable.{ :: => Cons }
  import test._

  type ABP = Apple :+: Banana :+: Pear :+: CNil
  type APBO = Apple :+: Banana :+: Orange :+: Pear :+: CNil

  type ABC = A.type :+: B.type :+: C.type :+: CNil

  @Test
  def testProductBasics {
    val p = Person("Joe Soap", "Brighton", 23)
    type SSI = String :: String :: Int :: HNil
    val gen = Generic[Person]

    val p0 = gen.to(p)
    typed[SSI](p0)
    assertEquals("Joe Soap" :: "Brighton" :: 23 :: HNil, p0)

    val p1 = gen.from(p0)
    typed[Person](p1)
    assertEquals(p, p1)
  }

  @Test
  def testProductVarargs {
    val p = PersonWithPseudonims("Joe Soap", "X", "M", "Z")
    val gen = Generic[PersonWithPseudonims]

    val p0 = gen.to(p)
    typed[String :: Seq[String] :: HNil](p0)
    assertEquals("Joe Soap" :: Seq("X", "M", "Z") :: HNil, p0)

    val p1 = gen.from(p0)
    typed[PersonWithPseudonims](p1)
    assertEquals(p, p1)
  }

  @Test
  def testTuples {
    val gen1 = Generic[Tuple1[Int]]
    typed[Generic[Tuple1[Int]] { type Repr = Int :: HNil }](gen1)

    val gen2 = Generic[(Int, String)]
    typed[Generic[(Int, String)] { type Repr = Int :: String :: HNil }](gen2)

    val gen3 = Generic[(Int, String, Boolean)]
    typed[Generic[(Int, String, Boolean)] { type Repr = Int :: String :: Boolean :: HNil }](gen3)
  }

  @Test
  def testProductMapBasics {
    val p = Person("Joe Soap", "Brighton", 23)

    val p0 = star(p)
    typed[Person](p0)
    assertEquals(Person("Joe Soap*", "Brighton*", 23), p0)
  }

  @Test
  def testProductNestedMap {
    val p = Person("Joe Soap", "Brighton", 23)
    val e = Employee(p, Salary(2000))

    val e0 = star(e)
    typed[Employee](e0)
    assertEquals(Employee(Person("Joe Soap*", "Brighton*", 23), Salary(2000)), e0)
  }

  @Test
  def testCoproductBasics {
    val a: Fruit = Apple()
    val p: Fruit = Pear()
    val b: Fruit = Banana()
    val o: Fruit = Orange()

    val gen = Generic[Fruit]

    val a0 = gen.to(a)
    typed[APBO](a0)

    val p0 = gen.to(p)
    typed[APBO](p0)

    val b0 = gen.to(b)
    typed[APBO](b0)

    val o0 = gen.to(o)
    typed[APBO](o0)

    val a1 = gen.from(a0)
    typed[Fruit](a1)

    val p1 = gen.from(p0)
    typed[Fruit](p1)

    val b1 = gen.from(b0)
    typed[Fruit](b1)

    val o1 = gen.from(o0)
    typed[Fruit](o1)
  }

  @Test
  def testCoproductMapBasics {
    val a: Fruit = Apple()
    val p: Fruit = Pear()
    val b: Fruit = Banana()
    val o: Fruit = Orange()

    val gen = Generic[Fruit]

    val a0 = gen.to(a)
    typed[APBO](a0)
    val a1 = a0.map(showFruit).unify
    assertEquals("Pomme", a1)

    val p0 = gen.to(p)
    typed[APBO](p0)
    val p1 = p0.map(showFruit).unify
    assertEquals("Poire", p1)

    val b0 = gen.to(b)
    typed[APBO](b0)
    val b1 = b0.map(showFruit).unify
    assertEquals("Banane", b1)

    val o0 = gen.to(o)
    typed[APBO](o0)
    val o1 = o0.map(showFruit).unify
    assertEquals("Orange", o1)
  }

  @Test
  def testSingletonCoproducts {
    type S = Single

    val gen = Generic[AbstractSingle]

    val s: AbstractSingle = Single()

    val s0 = gen.to(s)
    typed[Single :+: CNil](s0)

    val s1 = gen.from(s0)
    typed[AbstractSingle](s1)
  }

  @Test
  def testOverlappingCoproducts {
    val gen = Generic[Overlapping]
    val o: Overlapping = OAB(1)
    val o0 = gen.to(o)
    typed[OAB :+: OAC :+: OBC :+: CNil](o0)

    val o1 = gen.from(o0)
    typed[Overlapping](o1)
  }

  @Test
  def testCaseObjects {
    val a: Enum = A
    val b: Enum = B
    val c: Enum = C

    val gen = Generic[Enum]

    val a0 = gen.to(a)
    typed[ABC](a0)

    val b0 = gen.to(b)
    typed[ABC](b0)

    val c0 = gen.to(c)
    typed[ABC](c0)

    val a1 = gen.from(a0)
    typed[Enum](a1)

    val b1 = gen.from(b0)
    typed[Enum](b1)

    val c1 = gen.from(c0)
    typed[Enum](c1)
  }

  @Test
  def testCaseObjectMap {
    val a: Enum = A
    val b: Enum = B
    val c: Enum = C

    val gen = Generic[Enum]

    val a0 = gen.to(a)
    typed[ABC](a0)
    val a1 = a0.map(pickFruit)
    typed[ABP](a1)
    typed[Fruit](a1.unify)
    assertEquals(Apple(), a1.unify)

    val b0 = gen.to(b)
    typed[ABC](b0)
    val b1 = b0.map(pickFruit)
    typed[ABP](b1)
    typed[Fruit](b1.unify)
    assertEquals(Banana(), b1.unify)

    val c0 = gen.to(c)
    typed[ABC](c0)
    val c1 = c0.map(pickFruit)
    typed[ABP](c1)
    typed[Fruit](c1.unify)
    assertEquals(Pear(), c1.unify)
  }

  @Test
  def testParametrized {
    val t: Tree[Int] = Node(Node(Leaf(23), Leaf(13)), Leaf(11))
    type NI = Leaf[Int] :+: Node[Int] :+: CNil

    val gen = Generic[Tree[Int]]

    val t0 = gen.to(t)
    typed[NI](t0)

    val t1 = gen.from(t0)
    typed[Tree[Int]](t1)
  }

  @Test
  def testParametrizedWithVarianceOption {
    val o: Option[Int] = Option(23)
    type SN = None.type :+: Some[Int] :+: CNil

    val gen = Generic[Option[Int]]

    val o0 = gen.to(o)
    typed[SN](o0)

    val o1 = gen.from(o0)
    typed[Option[Int]](o1)
  }

  @Test
  def testMapOption {
    val o: Option[Int] = Option(23)

    val o0 = inc(o)
    typed[Option[Int]](o0)
    assertEquals(Some(24), o0)

    val oo: Option[Option[Int]] = Option(Option(23))
    val oo0 = inc(oo)
    typed[Option[Option[Int]]](oo0)
    assertEquals(Some(Some(24)), oo0)
  }

  @Test
  def testParametrizedWithVarianceList {
    import scala.collection.immutable.{ :: => Cons }

    val l: List[Int] = List(1, 2, 3)
    type CN = Cons[Int] :+: Nil.type :+: CNil

    val gen = Generic[List[Int]]

    val l0 = gen.to(l)
    typed[CN](l0)

    val l1 = gen.from(l0)
    typed[List[Int]](l1)
  }

  @Test
  def testParametrzedSubset {
    val l = Left(23)
    val r = Right(true)
    type IB = Left[Int] :+: Right[Boolean] :+: CNil

    val gen = Generic[Xor[Int, Boolean]]

    val c0 = gen.to(l)
    assertTypedEquals[IB](Inl(l), c0)

    val c1 = gen.to(r)
    assertTypedEquals[IB](Inr(Inl(r)), c1)
  }

  @Test
  def testParametrizedPermute {
    val s = Swap(23, true)
    type IB = Swap[Int, Boolean] :+: CNil

    val gen = Generic[Base[Boolean, Int]]

    val s0 = gen.to(s)
    assertTypedEquals[IB](Inl(s), s0)
  }

  @Test
  def testAbstractNonCC {
    val ncca = new NonCCA(23, "foo")
    val nccb = new NonCCB(true, 2.0)
    val ancc: AbstractNonCC = ncca

    val genA = Generic[NonCCA]
    val genB = Generic[NonCCB]
    val genAbs = Generic[AbstractNonCC]

    val rA = genA.to(ncca)
    assertTypedEquals[Int :: String :: HNil](23 :: "foo" :: HNil, rA)

    val rB = genB.to(nccb)
    assertTypedEquals[Boolean :: Double :: HNil](true :: 2.0 :: HNil, rB)

    val rAbs = genAbs.to(ancc)
    assertTypedEquals[NonCCA :+: NonCCB :+: CNil](Inl(ncca), rAbs)

    val fA = genA.from(13 :: "bar" :: HNil)
    typed[NonCCA](fA)
    assertEquals(13, fA.i)
    assertEquals("bar", fA.s)

    val fB = genB.from(false :: 3.0 :: HNil)
    typed[NonCCB](fB)
    assertEquals(false, fB.b)
    assertEquals(3.0, fB.d, Double.MinPositiveValue)

    val fAbs = genAbs.from(Inr(Inl(nccb)))
    typed[AbstractNonCC](fAbs)
    assertTrue(fAbs.isInstanceOf[NonCCB])
    assertEquals(true, fAbs.asInstanceOf[NonCCB].b)
    assertEquals(2.0, fAbs.asInstanceOf[NonCCB].d, Double.MinPositiveValue)
  }

  @Test
  def testNonCCWithCompanion {
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
  def testNonCCLazy {
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

  trait Parent {
    case class Nested(i: Int, s: String)
  }

  trait Child extends Parent {
    val gen = Generic[Nested]
  }

  object O extends Child

  @Test
  def testNestedInherited {
    val n0 = O.Nested(23, "foo")
    val repr = O.gen.to(n0)
    typed[Int :: String :: HNil](repr)
    val n1 = O.gen.from(repr)
    typed[O.Nested](n1)
    assertEquals(n0, n1)
  }

  @Test
  def testIsTuple {
    import record._
    import union._

    IsTuple[Unit]
    IsTuple[(Int, String)]

    illTyped(" IsTuple[HNil] ")
    illTyped(" IsTuple[Int :: String :: HNil] ")
    illTyped(" IsTuple[CNil] ")
    illTyped(" IsTuple[Int :+: String :+: CNil] ")
    illTyped(" IsTuple[A.type] ")
    illTyped(" IsTuple[Single] ")
    illTyped(" IsTuple[Person] ")
    illTyped(" IsTuple[Fruit] ")
    illTyped(" IsTuple[Record.`'i -> Int, 's -> String`.T] ")
    illTyped(" IsTuple[Union.`'i -> Int, 's -> String`.T] ")
    illTyped(" IsTuple[Int] ")
    illTyped(" IsTuple[String] ")
    illTyped(" IsTuple[Array[Int]] ")
  }

  @Test
  def testHasProductGeneric {
    import record._
    import union._

    HasProductGeneric[Single]
    HasProductGeneric[Person]
    HasProductGeneric[Unit]
    HasProductGeneric[(Int, String)]
    HasProductGeneric[A.type]
    HasProductGeneric[Single]
    HasProductGeneric[Person]

    illTyped(" HasProductGeneric[HNil] ")
    illTyped(" HasProductGeneric[Int :: String :: HNil] ")
    illTyped(" HasProductGeneric[CNil] ")
    illTyped(" HasProductGeneric[Int :+: String :+: CNil] ")
    illTyped(" HasProductGeneric[Fruit] ")
    illTyped(" HasProductGeneric[Record.`'i -> Int, 's -> String`.T] ")
    illTyped(" HasProductGeneric[Union.`'i -> Int, 's -> String`.T] ")
    illTyped(" HasProductGeneric[Int] ")
    illTyped(" HasProductGeneric[String] ")
    illTyped(" HasProductGeneric[Array[Int]] ")
  }

  @Test
  def testHasCoproductGeneric {
    import record._
    import union._

    HasCoproductGeneric[Fruit]

    illTyped(" HasCoproductGeneric[Unit] ")
    illTyped(" HasCoproductGeneric[(Int, String)] ")
    illTyped(" HasCoproductGeneric[HNil] ")
    illTyped(" HasCoproductGeneric[Int :: String :: HNil] ")
    illTyped(" HasCoproductGeneric[CNil] ")
    illTyped(" HasCoproductGeneric[Int :+: String :+: CNil] ")
    illTyped(" HasCoproductGeneric[A.type] ")
    illTyped(" HasCoproductGeneric[Single] ")
    illTyped(" HasCoproductGeneric[Person] ")
    illTyped(" HasCoproductGeneric[Record.`'i -> Int, 's -> String`.T] ")
    illTyped(" HasCoproductGeneric[Union.`'i -> Int, 's -> String`.T] ")
    illTyped(" HasCoproductGeneric[Int] ")
    illTyped(" HasCoproductGeneric[String] ")
    illTyped(" HasCoproductGeneric[Array[Int]] ")
  }

  @Test
  def testNonGeneric {
    import record._
    import union._

    illTyped(" Generic[Int] ")
    illTyped(" Generic[Array[Int]] ")
    illTyped(" Generic[String] ")
    illTyped(" Generic[HNil] ")
    illTyped(" Generic[Int :: String :: HNil] ")
    illTyped(" Generic[CNil] ")
    illTyped(" Generic[Int :+: String :+: CNil] ")
    illTyped(" Generic[Record.`'i -> Int, 's -> String`.T] ")
    illTyped(" Generic[Union.`'i -> Int, 's -> String`.T] ")
  }

  sealed trait Color
  case object Green extends Color
  object Color {
    case object Red extends Color
  }

  @Test
  def testNestedCaseObjects {
    Generic[Green.type]
    Generic[Color.Red.type]
    LabelledGeneric[Green.type]
    LabelledGeneric[Color.Red.type]
  }

  sealed trait Base1
  case object Foo1 extends Base1
  case object Bar1 extends Base1

  trait TC[T]

  object TC {
    def apply[T](implicit tc: TC[T]): TC[T] = tc

    implicit def hnilTC: TC[HNil] = new TC[HNil] {}
    implicit def hconsTC[H, T <: HList](implicit hd: Lazy[TC[H]], tl: Lazy[TC[T]]): TC[H :: T] = new TC[H :: T] {}

    implicit def cnilTC: TC[CNil] = new TC[CNil] {}
    implicit def cconsTC[H, T <: Coproduct](implicit hd: Lazy[TC[H]], tl: Lazy[TC[T]]): TC[H :+: T] = new TC[H :+: T] {}

    implicit def projectTC[F, G](implicit gen: Generic.Aux[F, G], tc: Lazy[TC[G]]): TC[F] = new TC[F] {}
  }

  @Test
  def testCaseObjectsAndLazy {
    TC[Base1]
  }
}

package GenericTestsAux2 {
  trait Foo[T]

  object Foo {
    implicit val deriveHNil: Foo[HNil] = ???

    implicit def deriveLabelledGeneric[A, Rec <: HList]
      (implicit gen: Generic.Aux[A, Rec], auto: Foo[Rec]): Foo[A] = ???
  }

  class Bar[A]

  object Bar {
    implicit def cnil: Bar[CNil] = ???

    implicit def deriveCoproduct[H, T <: Coproduct]
      (implicit headFoo: Foo[H], tailAux: Bar[T]): Bar[H :+: T] = ???

    implicit def labelledGeneric[A, U <: Coproduct]
      (implicit gen: Generic.Aux[A, U], auto: Bar[U]): Bar[A] = ???
  }

  class Outer1 {
    sealed trait Color
    object Inner {
      case object Red extends Color
    }

    implicitly[Bar[Color]]
  }

  object Outer2 {
    class Wrapper {
      sealed trait Color
    }
    val wrapper = new Wrapper
    import wrapper.Color
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    implicitly[Bar[Color]]
  }

  object Outer3 {
    class Wrapper {
      sealed trait Color
    }
    val wrapper = new Wrapper
    case object Red extends wrapper.Color
    case object Green extends wrapper.Color
    case object Blue extends wrapper.Color

    implicitly[Bar[wrapper.Color]]
  }

  object Outer4 {
    val wrapper = new Wrapper
    case object Red extends wrapper.Color
    case object Green extends wrapper.Color
    case object Blue extends wrapper.Color

    class Wrapper {
      sealed trait Color
      implicitly[Bar[wrapper.Color]]
    }
  }

  // This should compile correctly, however, the corresponding pattern match
  // falls foul of https://issues.scala-lang.org/browse/SI-9220
  /*
  object Outer5 {
    trait Command
    object Command {
      sealed trait Execution extends Command
    }

    case class Buzz() extends Command.Execution
    case class Door() extends Command.Execution

    Generic[Command.Execution]
  }
  */
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

object EnumDefns1 {
  sealed trait EnumVal
  object BarA extends EnumVal { val name = "A" }
  object BarB extends EnumVal { val name = "B" }
  object BarC extends EnumVal { val name = "C" }
}

object EnumDefns2 {
  sealed trait EnumVal
  case object BarA extends EnumVal { val name = "A" }
  case object BarB extends EnumVal { val name = "B" }
  case object BarC extends EnumVal { val name = "C" }
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

object EnumDefns5 {
  sealed trait EnumVal
  object EnumVal {
    object BarA extends EnumVal { val name = "A" }
    object BarB extends EnumVal { val name = "B" }
    object BarC extends EnumVal { val name = "C" }
  }
}

object EnumDefns6 {
  sealed trait EnumVal
  object EnumVal {
    case object BarA extends EnumVal { val name = "A" }
    case object BarB extends EnumVal { val name = "B" }
    case object BarC extends EnumVal { val name = "C" }
  }
}

object EnumDefns7 {
  sealed trait EnumVal
  object EnumVal {
    val BarA, BarB, BarC = new EnumVal {}
  }
}

class TestEnum {
  @Test
  def testEnum0 {
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
  def testEnum1 {
    import EnumDefns1._

    val gen = Generic[EnumVal]
    val a0 = gen.to(BarA)
    assert(a0 == Inl(BarA))

    val b0 = gen.to(BarB)
    assert(b0 == Inr(Inl(BarB)))

    val c0 = gen.to(BarC)
    assert(c0 == Inr(Inr(Inl(BarC))))
  }

  @Test
  def testEnum2 {
    import EnumDefns2._

    val gen = Generic[EnumVal]
    val a0 = gen.to(BarA)
    assert(a0 == Inl(BarA))

    val b0 = gen.to(BarB)
    assert(b0 == Inr(Inl(BarB)))

    val c0 = gen.to(BarC)
    assert(c0 == Inr(Inr(Inl(BarC))))
  }

  @Test
  def testEnum3 {
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
  def testEnum4 {
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
  def testEnum5 {
    import EnumDefns5._
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
  def testEnum6 {
    import EnumDefns6._
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
  def testEnum7 {
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
  trait Defs {
    case class CC(i: Int, s: String)

    sealed trait Sum
    case class SumI(i: Int) extends Sum
    case class SumS(s: String) extends Sum
  }

  object Defs extends Defs

  object Derivations {
    import shapeless._

    Generic[Defs.CC]
    Generic[Defs.SumI]
    Generic[Defs.SumS]

    Generic[Defs.Sum]
    Generic.materialize[Defs.Sum, Defs.SumI :+: Defs.SumS :+: CNil]
  }
}

package TestSingletonMembers {
  case class CC(i: Int, s: Witness.`"msg"`.T)

  object Derivations2 {
    Generic[CC]
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
  implicitly[gen1.Repr =:= (Atoms01.One :+: Atoms01.Zero :+: CNil)]

  val gen2 = Generic[Atoms02.Atom]
  implicitly[gen2.Repr =:= (Atoms02.Two :+: Atoms02.Zero :+: CNil)]
}

object PrivateCtorDefns {
  sealed trait PublicFamily
  case class PublicChild() extends PublicFamily
  private case class PrivateChild() extends PublicFamily
}

object PrivateCtor {
  import PrivateCtorDefns._

  illTyped("""
  Generic[Access.PublicFamily]
  """)
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
