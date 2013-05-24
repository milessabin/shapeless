/*
 * Copyright (c) 2013 Miles Sabin 
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
  
  trait starLP extends Poly1 {
    implicit def default[T] = at[T](identity)
  }

  object star extends starLP {
    implicit def caseString = at[String](_+"*")

    implicit def caseIso[T, L <: HList](implicit gen: GenericAux[T, L], mapper: MapperAux[this.type, L, L]) =
      at[T](t => gen.from(gen.to(t).map(star)))
  }
  
  trait incLP extends Poly1 {
    implicit def default[T] = at[T](identity)
  }
  
  object inc extends incLP {
    implicit val caseInt = at[Int](_+1)
    
    implicit def caseProduct[T, L <: HList](implicit gen: GenericAux[T, L], mapper: MapperAux[this.type, L, L]) =
      at[T](t => gen.from(gen.to(t).map(inc)))
      
    implicit def caseCoproduct[T, L <: Coproduct](implicit gen: GenericAux[T, L], mapper: CPMapperAux[this.type, L, L]) =
      at[T](t => gen.from(gen.to(t).map(inc)))
  }
}

class GenericTests {
  import GenericTestsAux._
  import scala.collection.immutable.{ :: => Cons }
  
  type ABP = Apple :+: Banana :+: Pear
  type APBO = Apple :+: Pear :+: Banana :+: Orange
  
  type ABC = A.type :+: B.type :+: C.type
  
  def typed[T](t : => T) {}

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
  def testTuples {
    val gen1 = Generic[Tuple1[Int]]
    typed[Generic[Tuple1[Int]] { type Repr = Int :: HNil }](gen1)
    
    val gen2 = Generic[(Int, String)]
    typed[Generic[(Int, String)] { type Repr = Int :: String :: HNil }](gen2)
    
    val gen3 = Generic[(Int, String, Boolean)]
    typed[Generic[(Int, String, Boolean)] { type Repr = Int :: String :: Boolean :: HNil }](gen3)
  }

  @Test
  def testHLists {
    type T0 = HNil
    type T1 = Int :: HNil
    type T2 = Int :: String :: HNil
    type T3 = Int :: String :: Boolean :: HNil
    
    val gen0 = Generic[HNil]
    typed[Generic[T0] { type Repr = T0 }](gen0)
    
    val gen1 = Generic[Int :: HNil]
    typed[Generic[T1] { type Repr = T1 }](gen1)
    
    val gen2 = Generic[Int :: String :: HNil]
    typed[Generic[T2] { type Repr = T2 }](gen2)
    
    val gen3 = Generic[Int :: String :: Boolean :: HNil]
    typed[Generic[T3] { type Repr = T3 }](gen3)
  }

  @Test
  def testProductMapBasics {
    val p = Person("Joe Soap", "Brighton", 23)
    
    val p0 = star(p)
    typed[Person](p0)
    assertEquals(Person("Joe Soap*", "Brighton*", 23), p0)
  }
  
//  @Test
//  def testProductNestedMap {
//    val p = Person("Joe Soap", "Brighton", 23)
//    val e = Employee(p, Salary(2000))
//    
//    val e0 = star(e)
//    typed[Employee](e0)
//    assertEquals(Employee(Person("Joe Soap*", "Brighton*", 23), Salary(2000)), e0)
//  }
  
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
    typed[Single](s0)
    
    val s1 = gen.from(s0)
    typed[AbstractSingle](s1)
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
    type NI = Node[Int] :+: Leaf[Int]
    
    val gen = Generic[Tree[Int]]
    
    val t0 = gen.to(t)
    typed[NI](t0)
    
    val t1 = gen.from(t0)
    typed[Tree[Int]](t1)
  }
  
  @Test
  def testParametrizedWithVarianceOption {
    val o: Option[Int] = Option(23)
    type SN = None.type :+: Some[Int] 
    
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
    type CN = Cons[Int] :+: Nil.type 
    
    val gen = Generic[List[Int]]
    
    val l0 = gen.to(l)
    typed[CN](l0)
    
    val l1 = gen.from(l0)
    typed[List[Int]](l1)
  }
}
