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
    implicit def caseApple: Case.Aux[Apple, String]   = at[Apple](_ => "Pomme")
    implicit def casePear: Case.Aux[Pear, String]     = at[Pear](_ => "Poire")
    implicit def caseBanana: Case.Aux[Banana, String] = at[Banana](_ => "Banane")
    implicit def caseOrange: Case.Aux[Orange, String] = at[Orange](_ => "Orange")
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
    implicit def caseA: Case.Aux[A.type, Apple] = at[A.type](_ => Apple())
    implicit def caseB: Case.Aux[B.type, Banana] = at[B.type](_ => Banana())
    implicit def caseC: Case.Aux[C.type, Pear] = at[C.type](_ => Pear())
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
    implicit def default[T]: Case.Aux[T, T] = at[T](identity)
  }

  object star extends starLP {
    implicit def caseString: Case.Aux[String, String] = at[String](_+"*")

    implicit def caseIso[T, L <: HList](implicit gen: Generic.Aux[T, L], mapper: => hl.Mapper.Aux[this.type, L, L]): Case.Aux[T, T] =
      at[T](t => gen.from(mapper(gen.to(t))))
  }

  trait incLP extends Poly1 {
    implicit def default[T]: Case.Aux[T, T] = at[T](identity)
  }

  object inc extends incLP {
    implicit val caseInt: Case.Aux[Int, Int] = at[Int](_+1)

    implicit def caseProduct[T, L <: HList](implicit gen: Generic.Aux[T, L], mapper: hl.Mapper.Aux[this.type, L, L]): Case.Aux[T, T] =
      at[T](t => gen.from(gen.to(t).map(inc)))

    implicit def caseCoproduct[T, L <: Coproduct](implicit gen: Generic.Aux[T, L], mapper: cp.Mapper.Aux[this.type, L, L]): Case.Aux[T, T] =
      at[T](t => gen.from(gen.to(t).map(inc)))
  }

  sealed trait Xor[+A, +B]
  case class Left[+LA](a: LA) extends Xor[LA, Nothing]
  case class Right[+RB](b: RB) extends Xor[Nothing, RB]

  sealed trait Base[BA, BB]
  case class Swap[SA, SB](a: SA, b: SB) extends Base[SB, SA]

  object SemiAuto {
    case object CObj
    object NonCObj
  }

  case class CCOrdered[A: Ordering](value: A)

  case class CCDegen(i: Int)()

  sealed trait Tap[A]
  final case class ConstTap[A](a: A) extends Tap[A]
  final case class InTap[A, -B](in: B => A) extends Tap[A]
  final case class OutTap[A, +B](out: A => B) extends Tap[A]
  final case class PipeTap[A, B](in: B => A, out: A => B) extends Tap[A]
}

class GenericTests {
  import GenericTestsAux._
  import scala.collection.immutable.{ :: => Cons }
  import test._

  type ABP = Apple :+: Banana :+: Pear :+: CNil
  type APBO = Apple :+: Pear :+: Banana :+: Orange :+: CNil

  type ABC = A.type :+: B.type :+: C.type :+: CNil

  @Test
  def testProductBasics: Unit = {
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
  def testProductVarargs: Unit = {
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
  def testTuples: Unit = {
    val gen1 = Generic[Tuple1[Int]]
    typed[Generic[Tuple1[Int]] { type Repr = Int :: HNil }](gen1)

    val gen2 = Generic[(Int, String)]
    typed[Generic[(Int, String)] { type Repr = Int :: String :: HNil }](gen2)

    val gen3 = Generic[(Int, String, Boolean)]
    typed[Generic[(Int, String, Boolean)] { type Repr = Int :: String :: Boolean :: HNil }](gen3)
  }

  @Test
  def testProductMapBasics: Unit = {
    val p = Person("Joe Soap", "Brighton", 23)

    val p0 = star(p)
    typed[Person](p0)
    assertEquals(Person("Joe Soap*", "Brighton*", 23), p0)
  }

  @Test
  def testProductNestedMap: Unit = {
    val p = Person("Joe Soap", "Brighton", 23)
    val e = Employee(p, Salary(2000))

    val e0 = star(e)
    typed[Employee](e0)
    assertEquals(Employee(Person("Joe Soap*", "Brighton*", 23), Salary(2000)), e0)
  }

  @Test
  def testCoproductBasics: Unit = {
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
  def testCoproductMapBasics: Unit = {
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
  def testSingletonCoproducts: Unit = {
    type S = Single

    val gen = Generic[AbstractSingle]

    val s: AbstractSingle = Single()

    val s0 = gen.to(s)
    typed[Single :+: CNil](s0)

    val s1 = gen.from(s0)
    typed[AbstractSingle](s1)
  }

  @Test
  def testCaseObjects: Unit = {
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

    val abc = ops.coproduct.LiftAll[ValueOf, gen.Repr]
    assertEquals(List(A, B, C), abc.instances.toList.map(_.value))
  }

  @Test
  def testCaseObjectMap: Unit = {
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
  def testParametrized: Unit = {
    val t: Tree[Int] = Node(Node(Leaf(23), Leaf(13)), Leaf(11))
    type NI = Node[Int] :+: Leaf[Int] :+: CNil

    val gen = Generic[Tree[Int]]

    val t0 = gen.to(t)
    typed[NI](t0)

    val t1 = gen.from(t0)
    typed[Tree[Int]](t1)
  }

  @Test
  def testParametrizedWithVarianceOption: Unit = {
    val o: Option[Int] = Option(23)
    type SN = None.type :+: Some[Int] :+: CNil

    val gen = Generic[Option[Int]]

    val o0 = gen.to(o)
    typed[SN](o0)

    val o1 = gen.from(o0)
    typed[Option[Int]](o1)
  }

  @Test
  def testMapOption: Unit = {
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
  def testParametrizedWithVarianceList: Unit = {
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
  def testParametrzedSubset: Unit = {
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
  def testParametrizedPermute: Unit = {
    val s = Swap(23, true)
    type IB = Swap[Int, Boolean] :+: CNil

    val gen = Generic[Base[Boolean, Int]]

    val s0 = gen.to(s)
    assertTypedEquals[IB](Inl(s), s0)
  }

  def use(@scala.annotation.unused v: Any): Unit = ()

  @Test
  def testIsTuple: Unit = {
    import record._
    import union._

    use(IsTuple[Unit])
    use(IsTuple[(Int, String)])

    illTyped(" IsTuple[HNil] ")
    illTyped(" IsTuple[Int :: String :: HNil] ")
    illTyped(" IsTuple[CNil] ")
    illTyped(" IsTuple[Int :+: String :+: CNil] ")
    illTyped(" IsTuple[A.type] ")
    illTyped(" IsTuple[Single] ")
    illTyped(" IsTuple[Person] ")
    illTyped(" IsTuple[Fruit] ")
    illTyped(""" IsTuple[("i" ->> Int) :: ("s" ->> String) :: HNil] """)
    illTyped(""" IsTuple[("i" ->> Int) :+: ("s" ->> String) :: CNil] """)
    illTyped(" IsTuple[Int] ")
    illTyped(" IsTuple[String] ")
    illTyped(" IsTuple[Array[Int]] ")
  }

  @Test
  def testHasProductGeneric: Unit = {
    import record._
    import union._

    use(HasProductGeneric[Single])
    use(HasProductGeneric[Person])
    use(HasProductGeneric[Unit])
    use(HasProductGeneric[(Int, String)])
    use(HasProductGeneric[A.type])
    use(HasProductGeneric[Single])
    use(HasProductGeneric[Person])

    illTyped(" HasProductGeneric[HNil] ")
    illTyped(" HasProductGeneric[Int :: String :: HNil] ")
    illTyped(" HasProductGeneric[CNil] ")
    illTyped(" HasProductGeneric[Int :+: String :+: CNil] ")
    illTyped(" HasProductGeneric[Fruit] ")
    illTyped(""" HasProductGeneric[("i" ->> Int) :: ("s" ->> String) :: HNil] """)
    illTyped(""" HasProductGeneric[("i" ->> Int) :+: ("s" ->> String) :: CNil] """)
    illTyped(" HasProductGeneric[Int] ")
    illTyped(" HasProductGeneric[String] ")
    illTyped(" HasProductGeneric[Array[Int]] ")
  }

  @Test
  def testHasCoproductGeneric: Unit = {
    import record._
    import union._

    use(HasCoproductGeneric[Fruit])

    illTyped(" HasCoproductGeneric[Unit] ")
    illTyped(" HasCoproductGeneric[(Int, String)] ")
    illTyped(" HasCoproductGeneric[HNil] ")
    illTyped(" HasCoproductGeneric[Int :: String :: HNil] ")
    illTyped(" HasCoproductGeneric[CNil] ")
    illTyped(" HasCoproductGeneric[Int :+: String :+: CNil] ")
    illTyped(" HasCoproductGeneric[A.type] ")
    illTyped(" HasCoproductGeneric[Single] ")
    illTyped(" HasCoproductGeneric[Person] ")
    illTyped(""" HasCoproductGeneric[("i" ->> Int) :: ("s" ->> String) :: HNil] """)
    illTyped(""" HasCoproductGeneric[("i" ->> Int) :+: ("s" ->> String) :: CNil] """)
    illTyped(" HasCoproductGeneric[Int] ")
    illTyped(" HasCoproductGeneric[String] ")
    illTyped(" HasCoproductGeneric[Array[Int]] ")
  }

  @Test
  def testNonGeneric: Unit = {
    import record._
    import union._

    illTyped(" Generic[Int] ")
    illTyped(" Generic[Array[Int]] ")
    illTyped(" Generic[String] ")
    illTyped(" Generic[HNil] ")
    illTyped(" Generic[Int :: String :: HNil] ")
    illTyped(" Generic[CNil] ")
    illTyped(" Generic[Int :+: String :+: CNil] ")
    illTyped(""" Generic[("i" ->> Int) :: ("s" ->> String) :: HNil] """)
    illTyped(""" Generic[("i" ->> Int) :+: ("s" ->> String) :: CNil] """)
  }

  sealed trait Color
  case object Green extends Color
  object Color {
    case object Red extends Color
  }

  @Test
  def testNestedCaseObjects: Unit = {
    use(Generic[Green.type])
    use(Generic[Color.Red.type])
    use(LabelledGeneric[Green.type])
    use(LabelledGeneric[Color.Red.type])
  }

  sealed trait Base1
  case object Foo1 extends Base1
  case object Bar1 extends Base1

  trait TC[T]

  object TC {
    def apply[T](implicit tc: TC[T]): TC[T] = tc

    implicit def hnilTC: TC[HNil] = new TC[HNil] {}
    implicit def hconsTC[H, T <: HList](implicit hd: => TC[H], tl: => TC[T]): TC[H :: T] = new TC[H :: T] {}

    implicit def cnilTC: TC[CNil] = new TC[CNil] {}
    implicit def cconsTC[H, T <: Coproduct](implicit hd: => TC[H], tl: => TC[T]): TC[H :+: T] = new TC[H :+: T] {}

    implicit def projectTC[F, G](implicit gen: Generic.Aux[F, G], tc: => TC[G]): TC[F] = new TC[F] {}
  }

  @Test
  def testCaseObjectsAndLazy: Unit = {
    use(TC[Base1])
  }

  @Test
  def testObjectSemiAuto: Unit = {
    import SemiAuto._
    val gen1 = Generic[CObj.type]
    val gen2 = Generic[NonCObj.type]

    assertTypedEquals[HNil](HNil, gen1.to(CObj))
    assertTypedEquals[CObj.type](CObj, gen1.from(HNil))
    assertTypedEquals[HNil](HNil, gen2.to(NonCObj))
    assertTypedEquals[NonCObj.type](NonCObj, gen2.from(HNil))
  }

  @Test
  def testPathViaObject: Unit = {
    sealed trait T
    object T {
      case class C(i: Int) extends T
      case object O extends T
    }

    type Repr = T.C :+: T.O.type :+: CNil
    val gen = Generic[T]
    val c = T.C(42)
    val injC: Repr = Inl(c)
    val injO: Repr = Inr(Inl(T.O))

    assertTypedEquals[Repr](injC, gen.to(c))
    assertTypedEquals[T](c, gen.from(injC))
    assertTypedEquals[Repr](injO, gen.to(T.O))
    assertTypedEquals[T](T.O, gen.from(injO))
  }

  @Test
  def testNoPathViaDef: Unit = {
    sealed trait T
    def t: T = {
      case class C(i: Int) extends T
      case object O extends T
      O
    }

    illTyped("Generic[T]")
  }

  @Test
  def testNoPathViaVal: Unit = {
    sealed trait T
    val t: T = {
      case class C(i: Int) extends T
      case object O extends T
      O
    }

    illTyped("Generic[T]")
  }

  @Test
  def testNoPathToAnonViaDef: Unit = {
    sealed trait T
    case object O extends T
    def t: T = new T { }

    illTyped("Generic[T]")
  }

  @Test
  def testCtorFieldsMismatch: Unit = {
    illTyped("Generic[Squared]")
  }

  def testCoproductWithFreeTypeParams: Unit = {
    val gen = Generic[Tap[String]]

    val const = ConstTap("simple")
    val in = InTap[String, Int](_.toString)
    val out = OutTap[String, Option[Char]](_.headOption)
    val pipe = PipeTap[String, Array[Byte]](new String(_), _.getBytes)

    val testData = List[(Tap[String], TapRepr[String])](
      const -> Inl(const),
      in -> Inr(Inl(in)),
      out -> Inr(Inr(Inl(out))),
      pipe -> Inr(Inr(Inr(Inl(pipe))))
    )

    for ((expected, actual) <- testData) {
      assertEquals(expected, gen.from(actual))
      assertEquals(gen.to(expected), actual)
    }
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

class TestEnum {

  @Test
  def testEnum1: Unit = {
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
  def testEnum2: Unit = {
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
  def testEnum5: Unit = {
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
  def testEnum6: Unit = {
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
  }
}

package TestSingletonMembers {
  case class CC(i: Int, s: "msg")

  object Derivations2 {
    Generic[CC]
  }
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

object HigherKinded {
  // https://github.com/milessabin/shapeless/issues/683
  type Id[A] = A

  sealed trait Foo[A[_]]
  case class Bar[A[_]]() extends Foo[A]

  Generic[Bar[Id]]
  Generic[Foo[Id]]

  sealed trait Pipo[A[_]]
  case class Lino() extends Pipo[Id]

  Generic[Pipo[Id]]
}
