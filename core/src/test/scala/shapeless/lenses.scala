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

package shapeless

import org.junit.Test
import org.junit.Assert._

import lens._, nat._, record._, syntax.singleton._, tag.@@, test._, testutil._

package lensTestDataTypes {
  sealed trait Sum1
  case class Prod1a(s2: Sum2, i: Int) extends Sum1
  case class Prod1b(s2: Sum2, s: String) extends Sum1

  sealed trait Sum2
  case class Prod2a(i: Int) extends Sum2
  case class Prod2b(s: String) extends Sum2

  sealed trait Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  case class Leaf[T](value: T) extends Tree[T]

  case class Foo(i: Int, s: String)
  case class Bar(i: Int, b: Boolean)

  case class Address(street : String, city : String, postcode : String)
  case class Person(name : String, age : Int, address : Address)

  sealed trait BGraph[T]
  case class BTerm[T](value: T) extends BGraph[T]

  class BNode[T](left0: => BGraph[T], right0: => BGraph[T]) extends BGraph[T] {
    lazy val left = left0
    lazy val right = right0
  }
}

import lensTestDataTypes._

trait LensTests {
  val address = Address("Southover Street", "Brighton", "BN2 9UA")
  val person = Person("Joe Grey", 37, address)

  val nameLens: Lens[Person, String]
  val ageLens: Lens[Person, Int]
  val addressLens: Lens[Person, Address]
  val streetLens: Lens[Person, String]
  val cityLens: Lens[Person, String]
  val postcodeLens: Lens[Person, String]

  @Test
  def testBasics {
    val age1 = ageLens.get(person)
    typed[Int](age1)
    assertEquals(37, age1)

    val person2 = ageLens.set(person)(38)
    assertEquals(38, person2.age)

    val street1 = streetLens.get(person)
    typed[String](street1)
    assertEquals("Southover Street", street1)

    val person3 = streetLens.set(person)("Montpelier Road")
    assertEquals("Montpelier Road", person3.address.street)
  }

  @Test
  def testCompose {
    val addressLens = lens[Person] >> 2
    val streetLens = lens[Address] >> 0

    val personStreetLens1 = streetLens compose addressLens
    val personStreetLens2 = compose(streetLens, addressLens)
    val personStreetLens3 = (streetLens :: addressLens :: HNil).reduceLeft(compose)

    val street1 = personStreetLens1.get(person)
    typed[String](street1)
    assertEquals("Southover Street", street1)

    val street2 = personStreetLens2.get(person)
    typed[String](street2)
    assertEquals("Southover Street", street2)

    val street3 = personStreetLens3.get(person)
    typed[String](street3)
    assertEquals("Southover Street", street3)
  }

  @Test
  def testTuples {
    type ISDB = (Int, (String, (Double, Boolean)))

    val tp = (23, ("foo", (2.0, false)))

    val lens0 = lens[ISDB] >> 0
    val lens1 = lens[ISDB] >> 1
    val lens10 = lens[ISDB] >> 1 >> 0
    val lens11 = lens[ISDB] >> 1 >> 1
    val lens110 = lens[ISDB] >> 1 >> 1 >> 0
    val lens111 = lens[ISDB] >> 1 >> 1 >> 1

    val i = lens0.get(tp)
    typed[Int](i)
    assertEquals(23, i)

    val tpi = lens0.set(tp)(13)
    typed[ISDB](tpi)
    assertEquals((13, ("foo", (2.0, false))), tpi)

    val sdb  = lens1.get(tp)
    typed[(String, (Double, Boolean))](sdb)
    assertEquals(("foo", (2.0, false)), sdb)

    val tpsdb = lens1.set(tp)("bar", (3.0, true))
    typed[ISDB](tpsdb)
    assertEquals((23, ("bar", (3.0, true))), tpsdb)

    val s = lens10.get(tp)
    typed[String](s)
    assertEquals("foo", s)

    val tps = lens10.set(tp)("bar")
    typed[ISDB](tps)
    assertEquals((23, ("bar", (2.0, false))), tps)

    val db  = lens11.get(tp)
    typed[(Double, Boolean)](db)
    assertEquals((2.0, false), db)

    val tpdb = lens11.set(tp)(3.0, true)
    typed[ISDB](tpdb)
    assertEquals((23, ("foo", (3.0, true))), tpdb)

    val d = lens110.get(tp)
    typed[Double](d)
    (2.0, d,  Double.MinPositiveValue)

    val tpd = lens110.set(tp)(3.0)
    typed[ISDB](tpd)
    assertEquals((23, ("foo", (3.0, false))), tpd)

    val b = lens111.get(tp)
    typed[Boolean](b)
    assertEquals(false, b)

    val tpb = lens111.set(tp)(true)
    typed[ISDB](tpb)
    assertEquals((23, ("foo", (2.0, true))), tpb)
  }

  @Test
  def testHLists {
    type ISB = Int :: String :: Boolean :: HNil
    val l = 23 :: "foo" :: true :: HNil

    val lens0 = hlistNthLens[ISB, _0]
    val lensI = hlistSelectLens[ISB, Int]
    val lens1 = hlistNthLens[ISB, _1]
    val lensS = hlistSelectLens[ISB, String]
    val lens2 = hlistNthLens[ISB, _2]
    val lensB = hlistSelectLens[ISB, Boolean]

    val i = lens0.get(l)
    typed[Int](i)
    assertEquals(23, i)
    assertEquals(23, lensI.get(l))

    val li = lens0.set(l)(13)
    typed[ISB](li)
    assertEquals(13 :: "foo" :: true :: HNil, li)
    assertEquals(13 :: "foo" :: true :: HNil, lensI.set(l)(13))

    val s = lens1.get(l)
    typed[String](s)
    assertEquals("foo", s)
    assertEquals("foo", lensS.get(l))

    val ls = lens1.set(l)("bar")
    typed[ISB](ls)
    assertEquals(23 :: "bar" :: true :: HNil, ls)
    assertEquals(23 :: "bar" :: true :: HNil, lensS.set(l)("bar"))

    val b = lens2.get(l)
    typed[Boolean](b)
    assertEquals(true, b)
    assertEquals(true, lensB.get(l))

    val lb = lens2.set(l)(false)
    typed[ISB](lb)
    assertEquals(23 :: "foo" :: false :: HNil, lb)
    assertEquals(23 :: "foo" :: false :: HNil, lensB.set(l)(false))
  }

  @Test
  def testRecords {
    import labelled.FieldType, syntax.singleton._

    val (fooT, barT) = (Witness("foo"), Witness("bar"))
    type LT = (fooT.T FieldType Int) :: (barT.T FieldType String) :: HNil
    val l = ("foo" ->> 42) :: ("bar" ->> "hi") :: HNil
    typed[LT](l)

    val li = recordLens[LT]("foo")
    assertEquals(42, li.get(l))
    assertEquals(("foo" ->> 84) :: ("bar" ->> "hi") :: HNil, li.set(l)(84))

    val ls = recordLens[LT]("bar")
    assertEquals("hi", ls.get(l))
    assertEquals(("foo" ->> 42) :: ("bar" ->> "bye") :: HNil, ls.set(l)("bye"))
  }

  @Test
  def testSets {
    val s = Set("foo", "bar", "baz")
    val lens = setLens[String]("bar")

    val b1 = lens.get(s)
    assert(b1)

    val s2 = lens.set(s)(false)
    assertEquals(Set("foo", "baz"), s2)

    val b2 = lens.get(s2)
    assert(!b2)

    val s3 = lens.set(s2)(true)
    assertEquals(s, s3)
  }

  @Test
  def testMaps {
    val m = Map(23 -> "foo", 13 -> "bar", 11 -> "baz")
    val lens = mapLens[Int, String](13)

    val s1 = lens.get(m)
    assertEquals(Option("bar"), s1)

    val m2 = lens.set(m)(Option("wibble"))
    assertEquals(Map(23 -> "foo", 13 -> "wibble", 11 -> "baz"), m2)

    val s2 = lens.get(m2)
    assertEquals(Option("wibble"), s2)

    val m3 = lens.set(m)(None)
    assertEquals(Map(23 -> "foo", 11 -> "baz"), m3)

    val s3 = lens.get(m3)
    assertEquals(None, s3)

    val m4 = lens.set(m3)(Option("bar"))
    assertEquals(m, m4)

    val s4 = lens.get(m4)
    assertEquals(Option("bar"), s4)
  }

  @Test
  def testProducts {
    val nameAgeCityLens = nameLens ~ ageLens ~ cityLens

    val nac1 = nameAgeCityLens.get(person)
    typed[(String, Int, String)](nac1)
    assertEquals(("Joe Grey", 37, "Brighton"), nac1)

    val person2 = nameAgeCityLens.set(person)("Joe Soap", 27, "London")
    assertEquals(Person("Joe Soap", 27, Address("Southover Street", "London", "BN2 9UA")), person2)
  }
}

class LensTestsNat extends LensTests {
  val nameLens     = lens[Person] >> 0
  val ageLens      = lens[Person] >> 1
  val addressLens  = lens[Person] >> 2
  val streetLens   = lens[Person] >> 2 >> 0
  val cityLens     = lens[Person] >> 2 >> 1
  val postcodeLens = lens[Person] >> 2 >> 2
}

class LensTestsKey extends LensTests {
  val nameLens     = lens[Person] >> 'name
  val ageLens      = lens[Person] >> 'age
  val addressLens  = lens[Person] >> 'address
  val streetLens   = lens[Person] >> 'address >> 'street
  val cityLens     = lens[Person] >> 'address >> 'city
  val postcodeLens = lens[Person] >> 'address >> 'postcode
}

class OpticTestsDynamic extends LensTests {
  val nameLens     = lens[Person].name
  val ageLens      = lens[Person].age
  val addressLens  = lens[Person].address
  val streetLens   = lens[Person].address.street
  val cityLens     = lens[Person].address.city
  val postcodeLens = lens[Person].address.postcode
}

class AscribedOpticTestsDynamic extends LensTests {
  val nameLens: Lens[Person, String]     = lens[Person].name
  val ageLens: Lens[Person, Int]         = lens[Person].age
  val addressLens: Lens[Person, Address] = lens[Person].address
  val streetLens: Lens[Person, String]   = lens[Person].address.street
  val cityLens: Lens[Person, String]     = lens[Person].address.city
  val postcodeLens: Lens[Person, String] = lens[Person].address.postcode
}

class OpticTests {
  @Test
  def testBasics {
    val s1: Sum1 = Prod1a(Prod2a(13), 23)
    val s2: Sum1 = Prod1b(Prod2b("foo"), "bar")

    val p1 = Prod1a(Prod2b("bar"), 11)

    val l1 = optic[Sum1][Prod1a]
    val l2 = optic[Sum1][Prod1a].i
    val l3 = optic[Sum1][Prod1a].s2
    val l4 = optic[Sum1][Prod1a].s2[Prod2a]
    val l5 = optic[Sum1][Prod1a].s2[Prod2a].i

    val g1 = l1.get(s1)
    typed[Option[Prod1a]](g1)
    assertEquals(Some(s1), g1)

    val g1b = l1.get(s2)
    typed[Option[Prod1a]](g1b)
    assertEquals(None, g1b)

    val g2 = l2.get(s1)
    typed[Option[Int]](g2)
    assertEquals(Some(23), g2)

    val g2b = l2.get(s2)
    typed[Option[Int]](g2b)
    assertEquals(None, g2b)

    val g3 = l3.get(s1)
    typed[Option[Sum2]](g3)
    assertEquals(Some(Prod2a(13)), g3)

    val g3b = l3.get(s2)
    typed[Option[Sum2]](g3b)
    assertEquals(None, g3b)

    val g4 = l4.get(s1)
    typed[Option[Prod2a]](g4)
    assertEquals(Some(Prod2a(13)), g4)

    val g4b = l4.get(s2)
    typed[Option[Prod2a]](g4b)
    assertEquals(None, g4b)

    val g5 = l5.get(s1)
    typed[Option[Int]](g5)
    assertEquals(Some(13), g5)

    val g5b = l5.get(s2)
    typed[Option[Int]](g5b)
    assertEquals(None, g5b)

    val t1 = l1.set(s1)(p1)
    typed[Sum1](t1)
    assertEquals(p1, t1)

    val t1b = l1.set(s2)(p1)
    typed[Sum1](t1b)
    assertEquals(p1, t1b)

    val t2 = l2.set(s1)(17)
    typed[Sum1](t2)
    assertEquals(Prod1a(Prod2a(13), 17), t2)

    val t2b = l2.set(s2)(17)
    typed[Sum1](t2b)
    assertEquals(s2, t2b)

    val t3 = l3.set(s1)(Prod2b("bar"))
    typed[Sum1](t3)
    assertEquals(Prod1a(Prod2b("bar"), 23), t3)

    val t3b = l3.set(s2)(Prod2b("bar"))
    typed[Sum1](t3b)
    assertEquals(s2, t3b)

    val t4 = l4.set(s1)(Prod2a(19))
    typed[Sum1](t4)
    assertEquals(Prod1a(Prod2a(19), 23), t4)

    val t4b = l4.set(s2)(Prod2a(19))
    typed[Sum1](t4b)
    assertEquals(s2, t4b)

    val t5 = l5.set(s1)(19)
    typed[Sum1](t5)
    assertEquals(Prod1a(Prod2a(19), 23), t5)

    val t5b = l5.set(s2)(19)
    typed[Sum1](t5b)
    assertEquals(s2, t5b)
  }

  @Test
  def testInferredProducts {
    val s1: Sum1 = Prod1a(Prod2a(13), 23)
    val s2: Sum1 = Prod1b(Prod2b("foo"), "bar")

    val p1 = Prod1a(Prod2b("bar"), 11)

    val li1 = optic[Sum1].i
    val li2 = optic[Sum1].s2
    val li3 = optic[Sum1].s2.i

    val g1 = li1.get(s1)
    typed[Option[Int]](g1)
    assertEquals(Some(23), g1)

    val g1b = li1.get(s2)
    typed[Option[Int]](g1b)
    assertEquals(None, g1b)

    val g2 = li2.get(s1)
    typed[Option[Sum2]](g2)
    assertEquals(Some(Prod2a(13)), g2)

    val g2b = li2.get(s2)
    typed[Option[Sum2]](g2b)
    assertEquals(None, g2b)

    val g3 = li3.get(s1)
    typed[Option[Int]](g3)
    assertEquals(Some(13), g3)

    val g3b = li3.get(s2)
    typed[Option[Int]](g3b)
    assertEquals(None, g3b)

    val t1 = li1.set(s1)(17)
    typed[Sum1](t1)
    assertEquals(Prod1a(Prod2a(13), 17), t1)

    val t1b = li1.set(s2)(17)
    typed[Sum1](t1b)
    assertEquals(s2, t1b)

    val t2 = li2.set(s1)(Prod2b("bar"))
    typed[Sum1](t2)
    assertEquals(Prod1a(Prod2b("bar"), 23), t2)

    val t2b = li2.set(s2)(Prod2b("bar"))
    typed[Sum1](t2b)
    assertEquals(s2, t2b)

    val t3 = li3.set(s1)(19)
    typed[Sum1](t3)
    assertEquals(Prod1a(Prod2a(19), 23), t3)

    val t3b = li3.set(s2)(19)
    typed[Sum1](t3b)
    assertEquals(s2, t3b)
  }

  @Test
  def testRecursive {
    val t1: Tree[Int] = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
    val t2: Tree[Int] = Node(Leaf(4), Node(Leaf(5), Leaf(6)))
    val t3: Node[Int] = Node(Leaf(7), Leaf(8))

    val l1 = optic[Tree[Int]]
    val l2 = optic[Tree[Int]][Node[Int]]
    val l3 = optic[Tree[Int]][Node[Int]].left
    val l4 = optic[Tree[Int]][Node[Int]].left[Node[Int]].right
    val l5 = optic[Tree[Int]][Node[Int]].left[Node[Int]].right[Leaf[Int]].value

    val g1 = l1.get(t1)
    typed[Tree[Int]](g1)
    assertEquals(t1, g1)

    val g1b = l1.get(t2)
    typed[Tree[Int]](g1b)
    assertEquals(t2, g1b)

    val g2 = l2.get(t1)
    typed[Option[Node[Int]]](g2)
    assertEquals(Some(t1), g2)

    val g2b = l2.get(t2)
    typed[Option[Node[Int]]](g2b)
    assertEquals(Some(t2), g2b)

    val g3 = l3.get(t1)
    typed[Option[Tree[Int]]](g3)
    assertEquals(Some(Node(Leaf(1), Leaf(2))), g3)

    val g4 = l4.get(t1)
    typed[Option[Tree[Int]]](g4)
    assertEquals(Some(Leaf(2)), g4)

    val g5 = l5.get(t1)
    typed[Option[Int]](g5)
    assertEquals(Some(2), g5)

    val s1 = l1.set(t1)(t3)
    typed[Tree[Int]](s1)
    assertEquals(t3, s1)

    val s1b = l1.set(t2)(t3)
    typed[Tree[Int]](s1b)
    assertEquals(t3, s1b)

    val s2 = l2.set(t1)(t3)
    typed[Tree[Int]](s2)
    assertEquals(t3, s2)

    val s2b = l2.set(t2)(t3)
    typed[Tree[Int]](s2b)
    assertEquals(t3, s2b)

    val s3 = l3.set(t1)(t3)
    typed[Tree[Int]](s3)
    assertEquals(Node(t3, Leaf(3)), s3)

    val s3b = l3.set(t2)(t3)
    typed[Tree[Int]](s3b)
    assertEquals(Node(t3, Node(Leaf(5), Leaf(6))), s3b)

    val s4 = l4.set(t1)(t3)
    typed[Tree[Int]](s4)
    assertEquals(Node(Node(Leaf(1), t3), Leaf(3)), s4)

    val s4b = l4.set(t2)(t3)
    typed[Tree[Int]](s4b)
    assertEquals(t2, s4b)

    val s5 = l5.set(t1)(23)
    typed[Tree[Int]](s5)
    assertEquals(Node(Node(Leaf(1), Leaf(23)), Leaf(3)), s5)

    val s5b = l5.set(t2)(23)
    typed[Tree[Int]](s5b)
    assertEquals(t2, s5b)
  }

  @Test
  def testRecursiveInferredProducts {
    val t1: Tree[Int] = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
    val t2: Tree[Int] = Node(Leaf(4), Node(Leaf(5), Leaf(6)))
    val t3: Node[Int] = Node(Leaf(7), Leaf(8))

    val l1 = optic[Tree[Int]]
    val l2 = optic[Tree[Int]].left
    val l3 = optic[Tree[Int]].left.right
    val l4 = optic[Tree[Int]].left.right.value

    val g1 = l1.get(t1)
    typed[Tree[Int]](g1)
    assertEquals(t1, g1)

    val g1b = l1.get(t2)
    typed[Tree[Int]](g1b)
    assertEquals(t2, g1b)

    val g2 = l2.get(t1)
    typed[Option[Tree[Int]]](g2)
    assertEquals(Some(Node(Leaf(1), Leaf(2))), g2)

    val g3 = l3.get(t1)
    typed[Option[Tree[Int]]](g3)
    assertEquals(Some(Leaf(2)), g3)

    val g4 = l4.get(t1)
    typed[Option[Int]](g4)
    assertEquals(Some(2), g4)

    val s1 = l1.set(t1)(t3)
    typed[Tree[Int]](s1)
    assertEquals(t3, s1)

    val s1b = l1.set(t2)(t3)
    typed[Tree[Int]](s1b)
    assertEquals(t3, s1b)

    val s2 = l2.set(t1)(t3)
    typed[Tree[Int]](s2)
    assertEquals(Node(t3, Leaf(3)), s2)

    val s2b = l2.set(t2)(t3)
    typed[Tree[Int]](s2b)
    assertEquals(Node(t3, Node(Leaf(5), Leaf(6))), s2b)

    val s3 = l3.set(t1)(t3)
    typed[Tree[Int]](s3)
    assertEquals(Node(Node(Leaf(1), t3), Leaf(3)), s3)

    val s3b = l3.set(t2)(t3)
    typed[Tree[Int]](s3b)
    assertEquals(t2, s3b)

    val s4 = l4.set(t1)(23)
    typed[Tree[Int]](s4)
    assertEquals(Node(Node(Leaf(1), Leaf(23)), Leaf(3)), s4)

    val s4b = l4.set(t2)(23)
    typed[Tree[Int]](s4b)
    assertEquals(t2, s4b)
  }

  @Test
  def testPaths {
    val t1: Tree[Int] = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
    val t2: Tree[Int] = Node(Leaf(4), Node(Leaf(5), Leaf(6)))
    val t3: Node[Int] = Node(Leaf(7), Leaf(8))

    val pi1 = ^
    val pi2 = ^.left
    val pi3 = ^.left.right
    val pi4 = ^.left.right.value

    val l1 = optic[Tree[Int]](pi1)
    val l2 = optic[Tree[Int]](pi2)
    val l3 = optic[Tree[Int]](pi3)
    val l4 = optic[Tree[Int]](pi4)

    val g1 = l1.get(t1)
    typed[Tree[Int]](g1)
    assertEquals(t1, g1)

    val g1b = l1.get(t2)
    typed[Tree[Int]](g1b)
    assertEquals(t2, g1b)

    val g2 = l2.get(t1)
    typed[Option[Tree[Int]]](g2)
    assertEquals(Some(Node(Leaf(1), Leaf(2))), g2)

    val g3 = l3.get(t1)
    typed[Option[Tree[Int]]](g3)
    assertEquals(Some(Leaf(2)), g3)

    val g4 = l4.get(t1)
    typed[Option[Int]](g4)
    assertEquals(Some(2), g4)

    val s1 = l1.set(t1)(t3)
    typed[Tree[Int]](s1)
    assertEquals(t3, s1)

    val s1b = l1.set(t2)(t3)
    typed[Tree[Int]](s1b)
    assertEquals(t3, s1b)

    val s2 = l2.set(t1)(t3)
    typed[Tree[Int]](s2)
    assertEquals(Node(t3, Leaf(3)), s2)

    val s2b = l2.set(t2)(t3)
    typed[Tree[Int]](s2b)
    assertEquals(Node(t3, Node(Leaf(5), Leaf(6))), s2b)

    val s3 = l3.set(t1)(t3)
    typed[Tree[Int]](s3)
    assertEquals(Node(Node(Leaf(1), t3), Leaf(3)), s3)

    val s3b = l3.set(t2)(t3)
    typed[Tree[Int]](s3b)
    assertEquals(t2, s3b)

    val s4 = l4.set(t1)(23)
    typed[Tree[Int]](s4)
    assertEquals(Node(Node(Leaf(1), Leaf(23)), Leaf(3)), s4)

    val s4b = l4.set(t2)(23)
    typed[Tree[Int]](s4b)
    assertEquals(t2, s4b)
  }

  @Test
  def testInferredLenses {
    def update[T, E](t: T)(e: E)(implicit mkLens: p.Lens[T, E]): T = mkLens().set(t)(e)

    val p = ^.i

    val foo = Foo(23, "foo")
    val bar = Bar(13, true)

    val foo2 = update(foo)(11)
    typed[Foo](foo2)
    assertEquals(Foo(11, "foo"), foo2)

    val bar2 = update(bar)(7)
    typed[Bar](bar2)
    assertEquals(Bar(7, true), bar2)
  }

  
  @Test
  def testUnapply {
    val t1: Tree[Int] = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
    val t2: Tree[Int] = Node(Leaf(4), Node(Leaf(5), Leaf(6)))

    val tOptic = optic[Tree[Int]]
    val llv = tOptic.left.left.value
    val lrv = tOptic.left.right.value
    val rv = tOptic.right.value

    val llv(x) = t1
    assertTypedEquals[Int](1, x)

    val lrv(y) = t1
    assertTypedEquals[Int](2, y)

    val rv(z) = t1
    assertTypedEquals[Int](3, z)

    val x2 = t2 match {
      case llv(x2) => Some(x2)
      case _ => None
    }
    assertTypedEquals[Option[Int]](None, x2)

    val y2 = t2 match {
      case lrv(y2) => Some(y2)
      case _ => None
    }
    assertTypedEquals[Option[Int]](None, y2)

    val z2 = t2 match {
      case rv(z2) => Some(z2)
      case _ => None
    }
    assertTypedEquals[Option[Int]](None, z2)

    val llvrv = llv ~ lrv ~ rv
    val llvrv(x3, y3, z3) = t1
    assertTypedEquals[Int](1, x3)
    assertTypedEquals[Int](2, y3)
    assertTypedEquals[Int](3, z3)
  }

  @Test
  def testLazyUnapply {
    val g = optic[BGraph[Int]]
    val l = g.left
    val rl = g.right.left
    val rll = rl ~ l
    val rlg = rl ~ g
    val rrlv = g.right.right.left.value
    val rrrv = g.right.right.right.value
    val rrlvrrrv = rrlv ~ rrrv
    val rrrlv = g.right.right.right.left.value
    val rrrrlv = g.right.right.right.right.left.value
    val looped = rrrlv ~ rrrrlv

    val rll(a, b) = new BNode(BTerm(1), new BNode(BTerm(2), BTerm(3)))
    assertEquals(BTerm(2), a)
    assertEquals(BTerm(1), b)

    lazy val g0 @ rll(x: BTerm[Int], y: BTerm[Int]) = new BNode(BTerm(1), new BNode(BTerm(2), new BNode(x, y)))
    val rrlvrrrv(x1, y1) = g0
    assertEquals(2, x1)
    assertEquals(1, y1)

    lazy val rlg(z: BTerm[Int], g1: BGraph[Int]) = new BNode(BTerm(1), new BNode(BTerm(2), new BNode(z, g1)))

    val looped(x2, y2) = g1
    assertEquals(1, x2)
    assertEquals(2, y2)
  }
}
