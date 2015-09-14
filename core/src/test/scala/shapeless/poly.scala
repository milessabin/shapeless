/*
 * Copyright (c) 2011-14 Miles Sabin 
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

import poly._
import ops.hlist.Mapper
import test._
import testutil._

/** Polymorphic singleton function. */
object singleton extends (Id ~> Set) {
  def apply[T](t : T) = Set(t)
}

/** Polymorphic function selecting an arbitrary element from a non-empty `Set`. */
object choose extends (Set ~> Option) {
  def apply[T](s : Set[T]) = s.headOption 
}

/** Polymorphic function creating singleton `List`s. */
object list extends (Id ~> List) {
  def apply[T](t : T) = List(t)
}

/** Polymorphic function returning the head of a `List`. */
object headOption extends (List ~> Option) {
  def apply[T](l : List[T]) = l.headOption
}

/** Polymorphic function which injects a value into an `Option`. */
object option extends (Id ~> Option) {
  def apply[T](t : T) = Option(t)
}

/** Polymorphic function testing whether or not an `Option` is defined. */
object isDefined extends (Option ~>> Boolean) {
  def apply[T](o : Option[T]) = o.isDefined
}

/** Polymorphic function which opens an `Option`. */
object get extends (Option ~> Id) {
  def apply[T](o : Option[T]) = o.get
}

/** Polymorphic addition with type specific cases. */
object plus extends Poly2 {
  implicit val caseInt = at[Int, Int](_ + _)
  implicit val caseDouble = at[Double, Double](_ + _)
  implicit val caseString = at[String, String](_ + _)
  implicit def caseList[T] = at[List[T], List[T]](_ ::: _)
}

/** Polymorphic zero with type specific cases. */
object zero extends Poly0 {
  implicit val zeroInt = at(0)
  implicit val zeroDouble = at(0.0)
  implicit val zeroString = at("")
  implicit def zeroList[T] = at[List[T]](Nil)
}

class PolyTests {
  object toInt extends (Id ~>> Int) {
    def apply[T](t : T) = t.toString.toInt
  }
  
  object size extends Poly1 {
    implicit def default[T] = at[T](_ => 1)
    implicit def caseInt = at[Int](_ => 1)
    implicit def caseString = at[String](_.length)
    implicit def caseList[T] = at[List[T]](_.length)
    implicit def caseOption[T](implicit st : Case.Aux[T, Int]) = at[Option[T]](t => 1+(t map size).getOrElse(0))
    implicit def caseTuple[T, U](implicit st : Case.Aux[T, Int], su : Case.Aux[U, Int]) = at[(T, U)]{ case (t, u) => size(t)+size(u) }
  }
  
  @Test
  def testHRFn {
    implicitly[choose.Case[Set[Int]]]
    
    implicitly[size.Case[Int]]

    implicitly[option.Case[Int]]

    implicitly[singleton.Case[Int]]

    val si = size(23)
    assertEquals(1, si)
    
    val ss = size("foo")
    assertEquals(3, ss)
    
    val sl = size(List(1, 2, 3))
    assertEquals(3, sl)
    
    val so = size(Option(23))
    assertEquals(2, so)

    val st = size((23, "foo"))
    assertEquals(4, st)
    
    val ls = List("foo", "bar", "baz")
    val lss = ls map size
    typed[List[Int]](lss)
    assertEquals(List(3, 3, 3), lss)
    
    val lsi = ls map identity
    typed[List[String]](lsi)
    assertEquals(ls, lsi)
    
    val is = identity("foo")
    typed[String](is)
    assertEquals("foo", is)

    // Direct application
    val s1 = singleton(23)
    typed[Set[Int]](s1)
    assertEquals(Set(23), s1)
    
    val s2 = singleton("foo")
    typed[Set[String]](s2)
    assertEquals(Set("foo"), s2)
    
    def app[G[_]](f : Int => G[Int]) = f(23)
    val as = app(singleton)
    typed[Set[Int]](as)
    assertEquals(Set(23), as)
    
    val al = app(list)
    typed[List[Int]](al)
    assertEquals(List(23), al)
    
    // Implicit conversion to monomorphic function values
    val l1 = List(1, 2, 3) map singleton
    typed[List[Set[Int]]](l1)
    assertEquals(List(Set(1), Set(2), Set(3)), l1)
    
    
    val l2 = List("foo", "bar", "baz") map list
    typed[List[List[String]]](l2)
    assertEquals(List(List("foo"), List("bar"), List("baz")), l2)
    
    val l3 = List(List(1), List(2), List(4)) map headOption
    typed[List[Option[Int]]](l3)
    assertEquals(List(Option(1), Option(2), Option(4)), l3)

    // Use as polymorphic function values
    def pairApply[G[_]](f : Id ~> G) = (f(23), f("foo"))
    
    val a1 = pairApply(singleton)
    typed[(Set[Int], Set[String])](a1)
    assertEquals((Set(23), Set("foo")), a1)
    
    val a2 = pairApply(list)
    typed[(List[Int], List[String])](a2)
    assertEquals((List(23), List("foo")), a2)

    // Use as polymorphic function values with type specific cases
    def pairApply2(f : Poly1)(implicit ci : f.Case[Int], cs : f.Case[String]) = (f(23), f("foo"))
    
    val a4 = pairApply2(singleton)
    typed[(Set[Int], Set[String])](a4)
    assertEquals((Set(23), Set("foo")), a4)
    
    val a5 = pairApply2(list)
    typed[(List[Int], List[String])](a5)
    assertEquals((List(23), List("foo")), a5)
    
    val a6 = pairApply2(size)
    typed[(Int, Int)](a6)
    assertEquals((1, 3), a6)

    def pairMap[G[_]](f : Id ~> G) = (List(1, 2, 3) map f, List("foo", "bar", "baz") map f)
    
    val m1 = pairMap(singleton)
    typed[(List[Set[Int]], List[Set[String]])](m1)
    assertEquals((List(Set(1), Set(2), Set(3)), List(Set("foo"), Set("bar"), Set("baz"))), m1)
    
    val m2 = pairMap(list)
    typed[(List[List[Int]], List[List[String]])](m2)
    assertEquals((List(List(1), List(2), List(3)), List(List("foo"), List("bar"), List("baz"))), m2)
    
    val l5 = List(1, 2, 3)
    val l6 = l5 map option
    typed[List[Option[Int]]](l6)
    assertEquals(List(Option(1), Option(2), Option(3)), l6)
    
    val l7 = l6 map isDefined
    typed[List[Boolean]](l7)
    assertEquals(List(true, true, true), l7)
    
    val lsi2 = List(Set(1), Set(2), Set(3))
    val loi2 = lsi2 map choose
    typed[List[Option[Int]]](loi2)
    assertEquals(List(Option(1), Option(2), Option(3)), loi2)

    val l8 = 23 :: "foo" :: List(1, 2, 3, 4) :: Option("bar") :: (23, "foo") :: 2.0 :: HNil
    val l9 = l8 map size
    typed[Int :: Int :: Int :: Int :: Int :: Int :: HNil](l9)
    assertEquals(1 :: 3 :: 4 :: 4 :: 4 :: 1 :: HNil, l9)

    def hlistMap(f : Poly)(implicit  mapper : Mapper[f.type, Int :: String :: HNil]) =
      (23 :: "foo" :: HNil) map f
      
    val hm1 = hlistMap(singleton)
    typed[Set[Int] :: Set[String] :: HNil](hm1)
    assertEquals(Set(23) :: Set("foo") :: HNil, hm1)

    val hm2 = hlistMap(list)
    typed[List[Int] :: List[String] :: HNil](hm2)
    assertEquals(List(23) :: List("foo") :: HNil, hm2)
  }

  @Test
  def testCompose {
    val so = singleton compose option
    
    val sos = so("foo")
    typed[Set[Option[String]]](sos)
    assertEquals(Set(Option("foo")), sos)

    val soi = so(23)
    typed[Set[Option[Int]]](soi)
    assertEquals(Set(Option(23)), soi)
  }

  @Test
  def testPolyVal {
    val i1 = zero[Int]
    typed[Int](i1)
    assertEquals(0, i1)
    
    val i2 = 23+zero[Int]
    typed[Int](i2)
    assertEquals(23, i2)
    
    val s1 = zero[String]
    typed[String](s1)
    assertEquals("", s1)
    
    val s2 = "foo"+zero[String]
    typed[String](s2)
    assertEquals("foo", s2)
    
    val l1 = zero[List[Int]]
    typed[List[Int]](l1)
    assertEquals(Nil, l1)

    val l2 = List(23)++zero[List[Int]]
    typed[List[Int]](l2)
    assertEquals(List(23), l2)
  }
  
  // Polymophic function value with type-specific cases for two
  // argument types. Result type is dependent on argument type
  object bidi extends Poly1 {
    implicit val caseInt = at[Int](_.toString)
    implicit val caseString = at[String](_.toInt)
  }

  @Test
  def testBinary {
    val bi = bidi(23)
    typed[String](bi)
    assertEquals("23", bi)
    
    val bs = bidi("23")
    typed[Int](bs)
    assertEquals(23, bs)
    
    val lis = 1 :: "2" :: 3 :: "4" :: HNil
    val blis = lis map bidi
    typed[String :: Int :: String :: Int :: HNil](blis)
    assertEquals("1" :: 2 :: "3" :: 4 :: HNil, blis)
  }

  @Test
  def testRotateLeft {
    object isd extends Poly3 {
      implicit val default = at[Int, String, Double] {
        case (i, s, d) => s"i: $i, s: $s, d: $d"
      }
    }

    val r1 = isd(1, "foo", 2.0)
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0", r1)

    val sdi = isd.rotateLeft[Nat._1]

    val r2 = sdi("foo", 2.0, 1)
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0", r2)

    val dis  = isd.rotateLeft[Nat._2]

    val r3 = dis(2.0, 1, "foo")
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0", r3)

    object isdc extends Poly4 {
      implicit val default = at[Int, String, Double, Char] {
        case (i, s, d, c) => s"i: $i, s: $s, d: $d, c: $c"
      }
    }

    val r4 = isdc(1, "foo", 2.0, 'a')
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0, c: a", r4)

    val sdci = isdc.rotateLeft[Nat._1]

    val r5 = sdci("foo", 2.0, 'a', 1)
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0, c: a", r5)

    val dcis = isdc.rotateLeft[Nat._2]

    val r6 = dcis(2.0, 'a', 1, "foo")
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0, c: a", r6)
  }

  @Test
  def testRotateRight {
    object isd extends Poly3 {
      implicit val default = at[Int, String, Double] {
        case (i, s, d) => s"i: $i, s: $s, d: $d"
      }
    }

    val r1 = isd(1, "foo", 2.0)
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0", r1)

    val dis = isd.rotateRight[Nat._1]

    val r2 = dis(2.0, 1, "foo")
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0", r2)

    val sdi  = isd.rotateRight[Nat._2]

    val r3 = sdi("foo", 2.0, 1)
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0", r3)

    object isdc extends Poly4 {
      implicit val default = at[Int, String, Double, Char] {
        case (i, s, d, c) => s"i: $i, s: $s, d: $d, c: $c"
      }
    }

    val r4 = isdc(1, "foo", 2.0, 'a')
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0, c: a", r4)

    val cisd = isdc.rotateRight[Nat._1]

    val r5 = cisd('a', 1, "foo", 2.0)
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0, c: a", r5)

    val dcis = isdc.rotateRight[Nat._2]

    val r6 = dcis(2.0, 'a', 1, "foo")
    assertTypedEquals[String]("i: 1, s: foo, d: 2.0, c: a", r6)
  }
}
