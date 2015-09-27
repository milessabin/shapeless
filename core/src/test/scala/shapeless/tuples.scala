/*
 * Copyright (c) 2013-14 Miles Sabin
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

import shapeless.test._
import testutil._

class TupleTests {
  import nat._
  import poly._
  import syntax.std.traversable._
  import syntax.std.tuple._
  import syntax.typeable._

  type SI = Tuple1[Set[Int]]
  type OI = Tuple1[Option[Int]]

  type SISS = (Set[Int], Set[String])
  type OIOS = (Option[Int], Option[String])

  type ISII = (Int, String, Int, Int)
  type IIII = (Int, Int, Int, Int)
  type IYII = (Int, Any, Int, Int)

  type OIOSOIOI = (Option[Int], Option[String], Option[Int], Option[Int])
  type SISSSISI = (Set[Int], Set[String], Set[Int], Set[Int])

  type BBBB = (Boolean, Boolean, Boolean, Boolean)

  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit

  type PWS = Product with Serializable with Fruit

  type YYYY = (Any, Any, Any, Any)
  type FF = (Fruit, Fruit)
  type AP = (Apple, Pear)
  type BP = (Banana, Pear)
  type AF = (Apple, Fruit)
  type FFFF = (Fruit, Fruit, Fruit, Fruit)
  type APAP = (Apple, Pear, Apple, Pear)
  type APBP = (Apple, Pear, Banana, Pear)
  type APB = (Apple, Pear, Banana)
  type PBPA = (Pear, Banana, Pear, Apple)
  type PABP = (Pear, Apple, Banana, Pear)

  val a : Apple = Apple()
  val p : Pear = Pear()
  val b : Banana = Banana()
  val f : Fruit = new Fruit {}

  val ap : AP = (a, p)
  val bp : BP = (b, p)
  val apap : APAP = (a, p, a, p)
  val apbp : APBP = (a, p, b, p)
  val apapList = List(a, p, a, p)
  val apbpList = List(a, p, b, p)
  val apapArray = Array(a, p, a, p)
  val apbpArray = Array(a, p, b, p)

  trait Ctv[-T]

  val ci: Ctv[Int] = new Ctv[Int] {}
  val cs: Ctv[String] = new Ctv[String] {}
  val cd: Ctv[Double] = new Ctv[Double] {}
  val cicscicicdList = List(ci, cs, ci, ci, cd)
  val cicscicicdArray = Array(ci, cs, ci, ci, cd)
  val cicscicicd = (ci, cs, ci, ci, cd)

  trait M[T]

  val mi: M[Int] = new M[Int] {}
  val ms: M[String] = new M[String] {}
  val md: M[Double] = new M[Double] {}
  val mimsmimimdList = List(mi, ms, mi, mi, md)
  val mimsmimimdArray = Array(mi, ms, mi, mi, md)
  val mimsmimimd = (mi, ms, mi, mi, md)

  import language.existentials
  val mExist: M[_] = new M[Double] {}
  val mimsmimemdList = List(mi, ms, mi, mExist, md)
  val mimsmimemdArray = Array[M[_]](mi, ms, mi, mExist, md)
  val mimsmimemd = (mi, ms, mi, mExist, md)

  trait M2[A,B]

  val m2i: M2[Int, Unit] = new M2[Int, Unit] {}
  val m2s: M2[String, Unit] = new M2[String, Unit] {}
  val m2d: M2[Double, Unit] = new M2[Double, Unit] {}
  val m2im2sm2im2im2dList = List(m2i, m2s, m2i, m2i, m2d)
  val m2im2sm2im2im2dArray = Array(m2i, m2s, m2i, m2i, m2d)
  val m2im2sm2im2im2d = (m2i, m2s, m2i, m2i, m2d)

  val m2iExist: M2[Int, _] = new M2[Int, Unit] {}
  val m2sExist: M2[String, _] = new M2[String, Unit] {}
  val m2dExist: M2[Double, _] = new M2[Double, Unit] {}
  val m2eim2esm2eim2eem2edList = List(m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)
  val m2eim2esm2eim2eem2edArray = Array(m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)
  val m2eim2esm2eim2eem2ed = (m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)

  object mkString extends (Any -> String)(_.toString)
  object fruit extends (Fruit -> Fruit)(f => f)

  trait incInt0 extends Poly1 {
    implicit def default[T] = at[T](t => ())
  }
  object incInt extends incInt0 {
    implicit val caseInt = at[Int](i => Tuple1(i+1))
  }

  trait extendedChoose0 extends Poly1 {
    implicit def default[T] = at[T](t => ())
  }
  object extendedChoose extends extendedChoose0 {
    implicit def caseSet[T] = at[Set[T]](s => Tuple1(s.headOption))
  }

  @Test
  def testBasics {
    val t = (1, "foo", 2.0)

    typed[Int](t.head)
    assertEquals(1, t.head)

    typed[String](t.tail.head)
    assertEquals("foo", t.tail.head)

    typed[Double](t.tail.tail.head)
    assertEquals(2.0, t.tail.tail.head, Double.MinPositiveValue)

    illTyped("""
      ().head
    """)

    illTyped("""
      ().tail
    """)

    illTyped("""
      t.tail.tail.tail.head
    """)
  }

  @Test
  def testMap {
    val s1 = Tuple1(Set(1))
    val o1 = s1 map choose
    typed[OI](o1)
    assertEquals(Tuple1(Option(1)), o1)

    val s2 = (Set(1), Set("foo"))
    val o2 = s2 map choose
    typed[OIOS](o2)
    assertEquals((Option(1), Option("foo")), o2)

    val l1 = (1, "foo", 2, 3)

    val l2 = l1 map singleton
    typed[SISSSISI](l2)
    assertEquals((Set(1), Set("foo"), Set(2), Set(3)), l2)

    val l3 = l1 map option
    typed[OIOSOIOI](l3)
    assertEquals((Option(1), Option("foo"), Option(2), Option(3)), l3)

    val l4 = (Option(1), Option("foo"), Option(2), Option(3))

    val l5 = l4 map get
    typed[ISII](l5)
    assertEquals((1, "foo", 2, 3), l5)

    typed[Int](l5.head)
    typed[String](l5.tail.head)
    typed[Int](l5.tail.tail.head)
    typed[Int](l5.tail.tail.tail.head)

    val l6 = l1 map identity
    typed[ISII](l6)
    assertEquals((1, "foo", 2, 3), l6)

    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals((true, true, true, true), l7)

    val l8 = (23, "foo", true)
    val l9 = l8 map mkString
    typed[(String, String, String)](l9)
    assertEquals(("23", "foo", "true"), l9)

    val l10 = apbp map fruit
    typed[(Fruit, Fruit, Fruit, Fruit)](l10)
    assertEquals(apbp, l10)

    val l11 = apbp map mkString
    typed[(String, String, String, String)](l11)
    assertEquals(("Apple()", "Pear()", "Banana()", "Pear()"), l11)
  }

  object dup extends Poly1 {
    implicit def default[T] = at[T](t => (t, t))
  }

  @Test
  def testFlatMap {
    val l1 = (1, "foo", true)

    val l2 = l1 flatMap dup
    typed[(Int, Int, String, String, Boolean, Boolean)](l2)
    assertEquals((1, 1, "foo", "foo", true, true), l2)

    val l3 = ((1, "foo"), (), (2.0, true), Tuple1("bar"))

    val l4 = l3 flatMap identity
    typed[(Int, String, Double, Boolean, String)](l4)
    assertEquals((1, "foo", 2.0, true, "bar"), l4)

    val l5 = (23, "foo", 7, true, 0)
    val l6 = l5 flatMap incInt
    typed[(Int, Int, Int)](l6)
    assertEquals((24, 8, 1), l6)

    val l7 = (Set(23), "foo", Set(true), 23)
    val l8 = l7 flatMap extendedChoose
    typed[(Option[Int], Option[Boolean])](l8)
    assertEquals((Option(23), Option(true)), l8)
  }

  @Test
  def testInitLast {
    val lp = apbp.last
    typed[Pear](lp)
    assertEquals(p, lp)

    val iapb = apbp.init
    typed[APB](iapb)
    assertEquals((a, p, b), iapb)
  }

  @Test
  def testReverse {
    val pbpa = apbp.reverse
    typed[PBPA](pbpa)
    assertEquals((p, b, p, a), pbpa)

    val al = Tuple1(a)
    val ral = al.reverse
    typed[Tuple1[Apple]](ral)
    assertEquals(Tuple1(a), ral)
  }

  @Test
  def testPrepend {
    val apbp2 = ap ::: bp
    typed[APBP](apbp2)
    assertEquals((a, p, b, p), apbp2)

    typed[Apple](apbp2.head)
    typed[Pear](apbp2.tail.head)
    typed[Banana](apbp2.tail.tail.head)
    typed[Pear](apbp2.tail.tail.tail.head)

    val pabp = ap reverse_::: bp
    typed[PABP](pabp)
    assertEquals((p, a, b, p), pabp)
  }

  @Test
  def testToSizedList {
    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val unit = ()
    val sunit = unit.toSized[List]
    assertEquals(0, sunit.length)
    val expectedUnsized = List.empty[Nothing]
    equalInferredTypes(expectedUnsized, sunit.unsized)
    assertEquals(expectedUnsized, sunit.unsized)

    val sizedApap = apap.toSized[List]
    assertEquals(Nat toInt apap.length, sizedApap.length)
    equalInferredTypes(apapList, sizedApap.unsized)
    assertEquals(apapList, sizedApap.unsized)

    val sizedApbp = apbp.toSized[List]
    assertEquals(Nat toInt apbp.length, sizedApbp.length)
    equalInferredTypes(apbpList, sizedApbp.unsized)
    assertEquals(apbpList, sizedApbp.unsized)

    val sizedCicscicicd = cicscicicd.toSized[List]
    assertEquals(Nat toInt cicscicicd.length, sizedCicscicicd.length)
    equalInferredTypes(cicscicicdList, sizedCicscicicd.unsized)
    assertEquals(cicscicicdList, sizedCicscicicd.unsized)

    val sizedMimsmimimd = mimsmimimd.toSized[List]
    assertEquals(Nat toInt mimsmimimd.length, sizedMimsmimimd.length)
    equalInferredTypes(mimsmimimdList, sizedMimsmimimd.unsized)
    assertEquals(mimsmimimdList, sizedMimsmimimd.unsized)

    val sizedMimsmimemd = mimsmimemd.toSized[List]
    assertEquals(Nat toInt mimsmimemd.length, sizedMimsmimemd.length)
    // equalInferredTypes(mimsmimemdList, sizedMimsmimemd.unsized)
    typed[List[M[_]]](sizedMimsmimemd.unsized)
    assertEquals(mimsmimemdList, sizedMimsmimemd.unsized)

    val sizedM2im2sm2im2im2d = m2im2sm2im2im2d.toSized[List]
    assertEquals(Nat toInt m2im2sm2im2im2d.length, sizedM2im2sm2im2im2d.length)
    equalInferredTypes(m2im2sm2im2im2dList, sizedM2im2sm2im2im2d.unsized)
    assertEquals(m2im2sm2im2im2dList, sizedM2im2sm2im2im2d.unsized)

    val sizedM2eim2esm2eim2eem2ed = m2eim2esm2eim2eem2ed.toSized[List]
    assertEquals(Nat toInt m2eim2esm2eim2eem2ed.length, sizedM2eim2esm2eim2eem2ed.length)
    // equalInferredTypes(m2eim2esm2eim2eem2edList, sizedM2eim2esm2eim2eem2ed.unsized)
    typed[List[M2[_ >: Double with Int with String, _]]](sizedM2eim2esm2eim2eem2ed.unsized)
    assertEquals(m2eim2esm2eim2eem2edList, sizedM2eim2esm2eim2eem2ed.unsized)
  }

  @Test
  def testToSizedArray {
    def assertArrayEquals2[T](arr1 : Array[T], arr2 : Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr2.asInstanceOf[Array[Object]])

    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val unit = ()
    val snil = unit.toSized[Array]
    assertEquals(Nat toInt unit.length, snil.length)
    val expectedUnsized = Array.empty[Nothing]
    equalInferredTypes(expectedUnsized, snil.unsized)
    assertArrayEquals2(expectedUnsized, snil.unsized)

    val sizedApap = apap.toSized[Array]
    assertEquals(Nat toInt apap.length, sizedApap.length)
    equalInferredTypes(apapArray, sizedApap.unsized)
    assertArrayEquals2(apapArray, sizedApap.unsized)

    val sizedApbp = apbp.toSized[Array]
    assertEquals(Nat toInt apbp.length, sizedApbp.length)
    equalInferredTypes(apbpArray, sizedApbp.unsized)
    assertArrayEquals2(apbpArray, sizedApbp.unsized)

    val sizedCicscicicd = cicscicicd.toSized[Array]
    assertEquals(Nat toInt cicscicicd.length, sizedCicscicicd.length)
    equalInferredTypes(cicscicicdArray, sizedCicscicicd.unsized)
    assertArrayEquals2(cicscicicdArray, sizedCicscicicd.unsized)

    val sizedMimsmimimd = mimsmimimd.toSized[Array]
    assertEquals(Nat toInt mimsmimimd.length, sizedMimsmimimd.length)
    equalInferredTypes(mimsmimimdArray, sizedMimsmimimd.unsized)
    assertArrayEquals2(mimsmimimdArray, sizedMimsmimimd.unsized)

    val sizedMimsmimemd = mimsmimemd.toSized[Array]
    assertEquals(Nat toInt mimsmimemd.length, sizedMimsmimemd.length)
    // equalInferredTypes(mimsmimemdArray, sizedMimsmimemd.unsized)
    // typed[Array[M[_]]](sizedMimsmimemd.unsized)
    // The line above compiles when mimsmimemd is an HList, not when it it a tuple...
    assertArrayEquals2(mimsmimemdArray.map(x => x: Any), sizedMimsmimemd.unsized.map(x => x: Any))

    val sizedM2im2sm2im2im2d = m2im2sm2im2im2d.toSized[Array]
    assertEquals(Nat toInt m2im2sm2im2im2d.length, sizedM2im2sm2im2im2d.length)
    equalInferredTypes(m2im2sm2im2im2dArray, sizedM2im2sm2im2im2d.unsized)
    assertArrayEquals2(m2im2sm2im2im2dArray, sizedM2im2sm2im2im2d.unsized)

    val sizedM2eim2esm2eim2eem2ed = m2eim2esm2eim2eem2ed.toSized[Array]
    assertEquals(Nat toInt m2eim2esm2eim2eem2ed.length, sizedM2eim2esm2eim2eem2ed.length)
    // equalInferredTypes(m2eim2esm2eim2eem2edArray, sizedM2eim2esm2eim2eem2ed.unsized)
    // typed[Array[M2[_ >: Double with Int with String, _]]](sizedM2eim2esm2eim2eem2ed.unsized)  // Same remark as above
    assertArrayEquals2(m2eim2esm2eim2eem2edArray.map(x => x: Any), sizedM2eim2esm2eim2eem2ed.unsized.map(x => x: Any))
  }

  @Test
  def testUnifier {
    import ops.tuple._

    implicitly[Unifier.Aux[Tuple1[Apple], Tuple1[Apple]]]
    implicitly[Unifier.Aux[(Fruit, Pear), (Fruit, Fruit)]]
    //implicitly[Unifier.Aux[(Apple, Pear), (Fruit, Fruit)]]

    implicitly[Unifier.Aux[(Int, String, Int, Int), YYYY]]

    val uapap = implicitly[Unifier.Aux[(Apple, Pear, Apple, Pear), (PWS, PWS, PWS, PWS)]]
    val unified1 = uapap(apap)
    typed[FFFF](unified1)
    val unified2 = apap.unify
    typed[FFFF](unified2)

    val ununified1 = unified2.cast[APAP]
    assertTrue(ununified1.isDefined)
    typed[APAP](ununified1.get)
    val ununified2 = unified2.cast[APBP]
    assertFalse(ununified2.isDefined)
    typed[Option[APBP]](ununified2)

    def getUnifier[T, Out](t : T)(implicit u: Unifier.Aux[T, Out]) = u

    val u2 = getUnifier(Tuple1(a))
    typed[Unifier.Aux[Tuple1[Apple], Tuple1[Apple]]](u2)
    val u3 = getUnifier((a, a))
    typed[Unifier.Aux[(Apple, Apple), (Apple, Apple)]](u3)
    val u4 = getUnifier((a, a, a))
    typed[Unifier.Aux[(Apple, Apple, Apple), (Apple, Apple, Apple)]](u4)
    val u5 = getUnifier((a, a, a, a))
    typed[Unifier.Aux[(Apple, Apple, Apple, Apple), (Apple, Apple, Apple, Apple)]](u5)
    //val u6 = getUnifier((a, p))
    //typed[Unifier.Aux[(Apple, Pear), (Fruit, Fruit)]](u6)
    val u7 = getUnifier((a, f))
    typed[Unifier.Aux[(Apple, Fruit), (Fruit, Fruit)]](u7)
    val u8 = getUnifier((f, a))
    typed[Unifier.Aux[(Fruit, Apple), (Fruit, Fruit)]](u8)
    val u9a = getUnifier((a, f))
    typed[Unifier.Aux[(Apple, Fruit), FF]](u9a)
    val u9b = getUnifier((a, p))
    typed[Unifier.Aux[(Apple, Pear), (PWS, PWS)]](u9b)
    val u10 = getUnifier(apap)
    typed[Unifier.Aux[APAP, (PWS, PWS, PWS, PWS)]](u10)
    val u11 = getUnifier(apbp)
    typed[Unifier.Aux[APBP, (PWS, PWS, PWS, PWS)]](u11)

    val invar1 = (Set(23), Set("foo"))
    val uinvar1 = invar1.unify
    typed[(Set[_ >: Int with String], Set[_ >: Int with String])](uinvar1)

    // Unifying three or more elements which have an invariant outer type constructor and differing type
    // arguments fails, presumably due to a failure to compute a sensible LUB.
    //val invar2 = (Set(23), Set("foo"), Set(true))
    //val uinvar2 = invar2.unify
  }

  @Test
  def testSubtypeUnifier {
    val fruits : (Apple, Pear, Fruit) = (a, p, f)
    typed[(Fruit, Fruit, Fruit)](fruits.unifySubtypes[Fruit])
    typed[(Apple, Pear, Fruit)](fruits.unifySubtypes[Apple])
    assertEquals((a, p, f), fruits.unifySubtypes[Fruit].filter[Fruit])

    val stuff : (Apple, String, Pear) = (a, "foo", p)
    typed[(Fruit, String, Fruit)](stuff.unifySubtypes[Fruit])
    assertEquals((), stuff.filter[Fruit])
    assertEquals((a, p), stuff.unifySubtypes[Fruit].filter[Fruit])
  }

  @Test
  def testToTraversableList {
    val empty = ().to[List]
    assertTypedEquals[List[Nothing]](Nil, empty)

    val fruits1 = apap.to[List]
    typed[List[Fruit]](fruits1)
    assertEquals(List(a, p, a, p), fruits1)

    val fruits2 = apbp.to[List]
    typed[List[Fruit]](fruits2)
    assertEquals(List(a, p, b, p), fruits2)

    val l1 = (1, "foo", 2, 3)

    val stuff = l1.to[List]
    typed[List[Any]](stuff)
    assertEquals(List(1, "foo", 2, 3), stuff)

    val l4 = (Option(1), Option("foo"), Option(2), Option(3))
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals((true, true, true, true), l7)

    val ll2 = l7.to[List]
    typed[Boolean](ll2.head)

    val moreStuff = (a, "foo", p).to[List]
    typed[List[Any]](moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val ctv = cicscicicd.to[List]
    equalInferredTypes(cicscicicdList, ctv)
    typed[List[Ctv[Int with String with Double]]](ctv)
    assertEquals(cicscicicdList, ctv)

    val m = mimsmimimd.to[List]
    equalInferredTypes(mimsmimimdList, m)
    typed[List[M[_ >: Int with String with Double]]](m)
    assertEquals(mimsmimimdList, m)

    val mWithEx = mimsmimemd.to[List]
    //  equalType(mimsmimemdList, mWithEx)
    typed[List[M[_]]](mWithEx)
    assertEquals(mimsmimemdList, mWithEx)

    val m2 = m2im2sm2im2im2d.to[List]
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    typed[List[M2[_ >: Int with String with Double, Unit]]](m2)
    assertEquals(m2im2sm2im2im2dList, m2)

    val m2e = m2eim2esm2eim2eem2ed.to[List]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    typed[List[M2[_ >: Int with String with Double, _]]](m2e)
    assertEquals(m2eim2esm2eim2eem2edList, m2e)
  }

  @Test
  def testToCoproduct {
    import ops.tuple._

    type PISB = (Int, String, Boolean)
    type CISBa = Int :+: String :+: Boolean :+: CNil
    type CISBb = the.`ToCoproduct[PISB]`.Out
    implicitly[CISBa =:= CISBb]
  }

  @Test
  def testToSum {
    import ops.tuple._
    
    type PISB = (Int, String, Boolean)
    type CISBa = Int :+: String :+: Boolean :+: CNil
    type SISBa = the.`ToSum[PISB]`.Out
    implicitly[CISBa =:= SISBa]

    type PIISSB = (Int, Int, String, String, Boolean)
    type SISBb = the.`ToSum[PIISSB]`.Out
    implicitly[CISBa =:= SISBb]
  }

  @Test
  def testToList {
    import ops.tuple.ToList

    ToList[Unit, Nothing]
    ToList[Unit, Int]
    ToList[APAP, Fruit]

    val empty = ().toList
    assertTypedEquals[List[Nothing]](Nil, empty)

    val fruits1 = apap.toList
    typed[List[Fruit]](fruits1)
    assertEquals(List(a, p, a, p), fruits1)

    val fruits2 = apbp.toList
    typed[List[Fruit]](fruits2)
    assertEquals(List(a, p, b, p), fruits2)

    val l1 = (1, "foo", 2, 3)

    val stuff = l1.toList
    typed[List[Any]](stuff)
    assertEquals(List(1, "foo", 2, 3), stuff)

    val l4 = (Option(1), Option("foo"), Option(2), Option(3))
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals((true, true, true, true), l7)

    val ll2 = l7.toList
    typed[Boolean](ll2.head)

    val moreStuff = (a, "foo", p).toList
    typed[List[Any]](moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val ctv = cicscicicd.toList
    equalInferredTypes(cicscicicdList, ctv)
    typed[List[Ctv[Int with String with Double]]](ctv)
    assertEquals(cicscicicdList, ctv)

    val m = mimsmimimd.toList
    equalInferredTypes(mimsmimimdList, m)
    typed[List[M[_ >: Int with String with Double]]](m)
    assertEquals(mimsmimimdList, m)

    val mWithEx = mimsmimemd.toList
    //  equalType(mimsmimemdList, mWithEx)
    typed[List[M[_]]](mWithEx)
    assertEquals(mimsmimemdList, mWithEx)

    val m2 = m2im2sm2im2im2d.toList
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    typed[List[M2[_ >: Int with String with Double, Unit]]](m2)
    assertEquals(m2im2sm2im2im2dList, m2)

    val m2e = m2eim2esm2eim2eem2ed.toList
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    typed[List[M2[_ >: Int with String with Double, _]]](m2e)
    assertEquals(m2eim2esm2eim2eem2edList, m2e)
  }

  @Test
  def testToTraversableArray {
    def assertArrayEquals2[T](arr1 : Array[T], arr2 : Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr2.asInstanceOf[Array[Object]])

    val empty = ().to[Array]
    typed[Array[Nothing]](empty)
    assertArrayEquals2(Array.empty, empty)

    val fruits1 = apap.to[Array].map(x => x : Fruit)
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.to[Array].map(x => x : Fruit)
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    val l1 = (1, "foo", 2, 3)

    val stuff = l1.to[Array]
    typed[Array[Any]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    val l4 = (Option(1), Option("foo"), Option(2), Option(3))
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals((true, true, true, true), l7)

    val ll2 = l7.to[Array]
    typed[Boolean](ll2(0))

    val moreStuff = (a, "foo", p).to[Array].map(x => x : AnyRef)
    typed[Array[AnyRef]](moreStuff)
    assertArrayEquals2(Array[AnyRef](a, "foo", p), moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val ctv = cicscicicd.to[Array]
    equalInferredTypes(cicscicicdArray, ctv)
    typed[Array[Ctv[Int with String with Double]]](ctv)
    assertArrayEquals2(cicscicicdArray, ctv)

    val m = mimsmimimd.to[Array]
    equalInferredTypes(mimsmimimdArray, m)
    typed[Array[M[_ >: Int with String with Double]]](m)
    assertArrayEquals2(mimsmimimdArray, m)

    val mWithEx = mimsmimemd.to[Array]
    // equalInferredTypes(mimsmimemdArray, mWithEx)
    // typed[Array[M[_]]](mWithEx)
    // The line above compiles when mimsmimemd is an HList, not when it it a tuple, as in testToSizedArray
    assertArrayEquals2(mimsmimemdArray.map(x => x : Any), mWithEx.map(x => x : Any))

    val m2 = m2im2sm2im2im2d.to[Array]
    equalInferredTypes(m2im2sm2im2im2dArray, m2)
    typed[Array[M2[_ >: Int with String with Double, Unit]]](m2)
    assertArrayEquals2(m2im2sm2im2im2dArray, m2)

    val m2e = m2eim2esm2eim2eem2ed.to[Array]
    // equalInferredTypes(m2eim2esm2eim2eem2edArray, m2e)
    // typed[Array[M2[_ >: Int with String with Double, _]]](m2e)
    // Same remark as above
    assertArrayEquals2(m2eim2esm2eim2eem2edArray.map(x => x : Any), m2e.map(x => x : Any))
  }

  @Test
  def testToArray {
    import ops.tuple.ToArray

    ToArray[Unit, Nothing]
    ToArray[Unit, Int]
    ToArray[APAP, Fruit]

    def assertArrayEquals2[T](arr1 : Array[T], arr2 : Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr2.asInstanceOf[Array[Object]])

    val empty = ().toArray
    typed[Array[Nothing]](empty)
    assertArrayEquals2(Array.empty, empty)

    val fruits1 = apap.toArray[Fruit]
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.toArray[Fruit]
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    val l1 = (1, "foo", 2, 3)

    val stuff = l1.toArray
    typed[Array[Any]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    val l4 = (Option(1), Option("foo"), Option(2), Option(3))
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals((true, true, true, true), l7)

    val ll2 = l7.toArray
    typed[Boolean](ll2(0))

    val moreStuff = (a, "foo", p).toArray[AnyRef]
    typed[Array[AnyRef]](moreStuff)
    assertArrayEquals2(Array[AnyRef](a, "foo", p), moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val ctv = cicscicicd.toArray
    equalInferredTypes(cicscicicdArray, ctv)
    typed[Array[Ctv[Int with String with Double]]](ctv)
    assertArrayEquals2(cicscicicdArray, ctv)

    val m = mimsmimimd.toArray
    equalInferredTypes(mimsmimimdArray, m)
    typed[Array[M[_ >: Int with String with Double]]](m)
    assertArrayEquals2(mimsmimimdArray, m)

    val mWithEx = mimsmimemd.toArray[M[_]]
    //  equalInferredTypes(mimsmimemdArray, mWithEx)
    typed[Array[M[_]]](mWithEx)
    assertArrayEquals2(mimsmimemdArray, mWithEx)

    val m2 = m2im2sm2im2im2d.toArray
    equalInferredTypes(m2im2sm2im2im2dArray, m2)
    typed[Array[M2[_ >: Int with String with Double, Unit]]](m2)
    assertArrayEquals2(m2im2sm2im2im2dArray, m2)

    val m2e = m2eim2esm2eim2eem2ed.toArray
    // equalInferredTypes(m2eim2esm2eim2eem2edArray, m2e)
    // typed[Array[M2[_ >: Int with String with Double, _]]](m2e)
    // The line above compiles when mimsmimemd is an HList, not when it is a tuple...
    assertArrayEquals2(m2eim2esm2eim2eem2edArray.map(x => x : Any), m2e.map(x => x : Any))
  }

  @Test
  def testFoldMap {
    val tl1 = (Option(1), Option("foo"), Option(2), Option(3))
    val tl2 = (Option(1), Option("foo"), (None : Option[Int]), Option(3))

    val mlfl1 = (tl1 map isDefined).toList.foldLeft(true)(_ && _)
    assertTrue(mlfl1)
    val mlfl2 = (tl2 map isDefined).toList.foldLeft(true)(_ && _)
    assertFalse(mlfl2)

    val fl1 = tl1.foldMap(true)(isDefined)(_ && _)
    assertTrue(fl1)
    val fl2 = tl2.foldMap(true)(isDefined)(_ && _)
    assertFalse(fl2)
  }

  @Test
  def testAt {
    val sn1 = (23, 3.0, "foo", (), "bar", true, 5L)

    val at0 = sn1(_0)
    typed[Int](at0)
    assertEquals(23, at0)

    val at1 = sn1(_1)
    typed[Double](at1)
    assertEquals(3.0, at1, Double.MinPositiveValue)

    val at2 = sn1(_2)
    typed[String](at2)
    assertEquals("foo", at2)

    val at3 = sn1(_3)
    typed[Unit](at3)
    assertEquals((), at3)

    val at4 = sn1(_4)
    typed[String](at4)
    assertEquals("bar", at4)

    val at5 = sn1(_5)
    typed[Boolean](at5)
    assertEquals(true, at5)

    val at6 = sn1(_6)
    typed[Long](at6)
    assertEquals(5L, at6)

    val sn2 = (
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21
    )

    val at21 = sn2(_21)
    typed[Int](at21)
    assertEquals(21, at21)
  }

  @Test
  def testAtLiteral {
    val sn1 = (23, 3.0, "foo", (), "bar", true, 5L)

    val at0 = sn1(0)
    typed[Int](at0)
    assertEquals(23, at0)

    val at1 = sn1(1)
    typed[Double](at1)
    assertEquals(3.0, at1, Double.MinPositiveValue)

    val at2 = sn1(2)
    typed[String](at2)
    assertEquals("foo", at2)

    val at3 = sn1(3)
    typed[Unit](at3)
    assertEquals((), at3)

    val at4 = sn1(4)
    typed[String](at4)
    assertEquals("bar", at4)

    val at5 = sn1(5)
    typed[Boolean](at5)
    assertEquals(true, at5)

    val at6 = sn1(6)
    typed[Long](at6)
    assertEquals(5L, at6)

    val sn2 = (
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21
    )

    val at21 = sn2(21)
    typed[Int](at21)
    assertEquals(21, at21)
  }

  @Test
  def testTakeDrop {
    val sn1 = (23, 3.0, "foo", (), "bar", true, 5L)

    val t0 = sn1.take(_0)
    typed[Unit](t0)
    assertEquals((), t0)

    val d0 = sn1.drop(_0)
    typed[(Int, Double, String, Unit, String, Boolean, Long)](d0)
    assertEquals((23, 3.0, "foo", (), "bar", true, 5L), d0)

    val t2 = sn1.take(_2)
    typed[(Int, Double)](t2)
    assertEquals((23, 3.0), t2)

    val d2 = sn1.drop(_2)
    typed[(String, Unit, String, Boolean, Long)](d2)
    assertEquals(("foo", (), "bar", true, 5L), d2)

    val t7 = sn1.take(_7)
    typed[(Int, Double, String, Unit, String, Boolean, Long)](t7)
    assertEquals((23, 3.0, "foo", (), "bar", true, 5L), t7)

    val d7 = sn1.drop(_7)
    typed[Unit](d7)
    assertEquals((), d7)
  }

  @Test
  def testTakeDropLiteral {
    val sn1 = (23, 3.0, "foo", (), "bar", true, 5L)

    val t0 = sn1.take(0)
    typed[Unit](t0)
    assertEquals((), t0)

    val d0 = sn1.drop(0)
    typed[(Int, Double, String, Unit, String, Boolean, Long)](d0)
    assertEquals((23, 3.0, "foo", (), "bar", true, 5L), d0)

    val t2 = sn1.take(2)
    typed[(Int, Double)](t2)
    assertEquals((23, 3.0), t2)

    val d2 = sn1.drop(2)
    typed[(String, Unit, String, Boolean, Long)](d2)
    assertEquals(("foo", (), "bar", true, 5L), d2)

    val t7 = sn1.take(7)
    typed[(Int, Double, String, Unit, String, Boolean, Long)](t7)
    assertEquals((23, 3.0, "foo", (), "bar", true, 5L), t7)

    val d7 = sn1.drop(7)
    typed[Unit](d7)
    assertEquals((), d7)
  }

  @Test
  def testSplit {
    val sn1 = (23, 3.0, "foo", (), "bar", true, 5L)

    val sni0 = sn1.split(_0)
    typed[(Unit, (Int, Double, String, Unit, String, Boolean, Long))](sni0)
    val sni1 = sn1.split(_1)
    typed[(Tuple1[Int], (Double, String, Unit, String, Boolean, Long))](sni1)
    val sni2 = sn1.split(_2)
    typed[((Int, Double), (String, Unit, String, Boolean, Long))](sni2)
    val sni3 = sn1.split(_3)
    typed[((Int, Double, String), (Unit, String, Boolean, Long))](sni3)
    val sni4 = sn1.split(_4)
    typed[((Int, Double, String, Unit), (String, Boolean, Long))](sni4)
    val sni5 = sn1.split(_5)
    typed[((Int, Double, String, Unit, String), (Boolean, Long))](sni5)
    val sni6 = sn1.split(_6)
    typed[((Int, Double, String, Unit, String, Boolean), Tuple1[Long])](sni6)
    val sni7 = sn1.split(_7)
    typed[((Int, Double, String, Unit, String, Boolean, Long), Unit)](sni7)

    val snri0 = sn1.reverse_split(_0)
    typed[(Unit, (Int, Double, String, Unit, String, Boolean, Long))](snri0)
    val snri1 = sn1.reverse_split(_1)
    typed[(Tuple1[Int], (Double, String, Unit, String, Boolean, Long))](snri1)
    val snri2 = sn1.reverse_split(_2)
    typed[((Double, Int), (String, Unit, String, Boolean, Long))](snri2)
    val snri3 = sn1.reverse_split(_3)
    typed[((String, Double, Int), (Unit, String, Boolean, Long))](snri3)
    val snri4 = sn1.reverse_split(_4)
    typed[((Unit, String, Double, Int), (String, Boolean, Long))](snri4)
    val snri5 = sn1.reverse_split(_5)
    typed[((String, Unit, String, Double, Int), (Boolean, Long))](snri5)
    val snri6 = sn1.reverse_split(_6)
    typed[((Boolean, String, Unit, String, Double, Int), Tuple1[Long])](snri6)
    val snri7 = sn1.reverse_split(_7)
    typed[((Long, Boolean, String, Unit, String, Double, Int), Unit)](snri7)
  }

  @Test
  def testSplitLiteral {
    val sn1 = (23, 3.0, "foo", (), "bar", true, 5L)

    val sni0 = sn1.split(0)
    typed[(Unit, (Int, Double, String, Unit, String, Boolean, Long))](sni0)
    val sni1 = sn1.split(1)
    typed[(Tuple1[Int], (Double, String, Unit, String, Boolean, Long))](sni1)
    val sni2 = sn1.split(2)
    typed[((Int, Double), (String, Unit, String, Boolean, Long))](sni2)
    val sni3 = sn1.split(3)
    typed[((Int, Double, String), (Unit, String, Boolean, Long))](sni3)
    val sni4 = sn1.split(4)
    typed[((Int, Double, String, Unit), (String, Boolean, Long))](sni4)
    val sni5 = sn1.split(5)
    typed[((Int, Double, String, Unit, String), (Boolean, Long))](sni5)
    val sni6 = sn1.split(6)
    typed[((Int, Double, String, Unit, String, Boolean), Tuple1[Long])](sni6)
    val sni7 = sn1.split(7)
    typed[((Int, Double, String, Unit, String, Boolean, Long), Unit)](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(Unit, (Int, Double, String, Unit, String, Boolean, Long))](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[(Tuple1[Int], (Double, String, Unit, String, Boolean, Long))](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[((Double, Int), (String, Unit, String, Boolean, Long))](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[((String, Double, Int), (Unit, String, Boolean, Long))](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[((Unit, String, Double, Int), (String, Boolean, Long))](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[((String, Unit, String, Double, Int), (Boolean, Long))](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[((Boolean, String, Unit, String, Double, Int), Tuple1[Long])](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[((Long, Boolean, String, Unit, String, Double, Int), Unit)](snri7)
  }

  @Test
  def testSelect {
    val sl = (1, true, "foo", 2.0)
    val si = sl.select[Int]
    typed[Int](si)
    assertEquals(1, si)

    val sb = sl.select[Boolean]
    typed[Boolean](sb)
    assertEquals(true, sb)

    val ss = sl.select[String]
    typed[String](ss)
    assertEquals("foo", ss)

    val sd = sl.select[Double]
    typed[Double](sd)
    assertEquals(2.0, sd, Double.MinPositiveValue)
  }

  @Test
  def testFilter {
    val l1 = (1, 2)
    val f1 = l1.filter[Int]
    typed[(Int, Int)](f1)
    assertEquals((1, 2), f1)

    val l2 = (1, true, "foo", 2)
    val f2 = l2.filter[Int]
    typed[(Int, Int)](f2)
    assertEquals((1, 2), f2)

    typed[Unit](l2.filter[Double])
  }

  @Test
  def testFilterNot {
    val l1 = (1, 2)
    val f1 = l1.filterNot[String]
    typed[(Int, Int)](f1)
    assertEquals((1, 2), f1)

    val l2 = (1, true, "foo", 2)
    val f2 = l2.filterNot[String]
    typed[(Int, Boolean, Int)](f2)
    assertEquals((1, true, 2), f2)

    typed[Unit](l1.filterNot[Int])
  }

  @Test
  def testReplace {
    val sl = (1, true, "foo", 2.0)

    val (i, r1) = sl.replace(23)
    typed[Int](i)
    assertEquals(1, i)
    assertEquals((23, true, "foo", 2.0), r1)

    val (b, r2) = sl.replace(false)
    typed[Boolean](b)
    assertEquals(true, b)
    assertEquals((1, false, "foo", 2.0), r2)

    val (s, r3) = sl.replace("bar")
    typed[String](s)
    assertEquals("foo", s)
    assertEquals((1, true, "bar", 2.0), r3)

    val (d, r4) = sl.replace(3.0)
    typed[Double](d)
    assertEquals(2.0, d, Double.MinPositiveValue)
    assertEquals((1, true, "foo", 3.0), r4)

    val (i2, r5) = sl.replaceType[Int]('*')
    typed[Int](i2)
    typed[Char](r5(_0))
    assertEquals(1, i2)
    assertEquals(('*', true, "foo", 2.0), r5)

    val (b2, r6) = sl.replaceType[Boolean]('*')
    typed[Boolean](b2)
    typed[Char](r6(_1))
    assertEquals(true, b2)
    assertEquals((1, '*', "foo", 2.0), r6)

    val (s2, r7) = sl.replaceType[String]('*')
    typed[String](s2)
    typed[Char](r7(_2))
    assertEquals("foo", s2)
    assertEquals((1, true, '*', 2.0), r7)

    val (d2, r8) = sl.replaceType[Double]('*')
    typed[Double](d2)
    typed[Char](r8(_3))
    assertEquals(2.0, d2, Double.MinPositiveValue)
    assertEquals((1, true, "foo", '*'), r8)

    val fruits = (a, p, a, f)

    val (x1, rr1) = fruits.replaceType[Pear](a)
    typed[Pear](x1)
    typed[(Apple, Apple, Apple, Fruit)](rr1)

    val (x2, rr2) = fruits.replaceType[Pear](f)
    typed[Pear](x2)
    typed[(Apple, Fruit, Apple, Fruit)](rr2)

    val (x3, rr3) = fruits.replaceType[Fruit](p)
    typed[Fruit](x3)
    typed[(Apple, Pear, Apple, Pear)](rr3)

    val (x4, rr4) = fruits.replace(p)
    typed[Pear](x4)
    typed[(Apple, Pear, Apple, Fruit)](rr4)

    val (x5, rr5) = fruits.replace(f)
    typed[Fruit](x5)
    typed[(Apple, Pear, Apple, Fruit)](rr5)
  }

  @Test
  def testUpdate {
    val sl = (1, true, "foo", 2.0)

    val r1 = sl.updatedElem(23)
    assertEquals((23, true, "foo", 2.0), r1)

    val r2 = sl.updatedElem(false)
    assertEquals((1, false, "foo", 2.0), r2)

    val r3 = sl.updatedElem("bar")
    assertEquals((1, true, "bar", 2.0), r3)

    val r4 = sl.updatedElem(3.0)
    assertEquals((1, true, "foo", 3.0), r4)

    val r5 = sl.updatedType[Int]('*')
    assertEquals(('*', true, "foo", 2.0), r5)

    val r6 = sl.updatedType[Boolean]('*')
    assertEquals((1, '*', "foo", 2.0), r6)

    val r7 = sl.updatedType[String]('*')
    assertEquals((1, true, '*', 2.0), r7)

    val r8 = sl.updatedType[Double]('*')
    assertEquals((1, true, "foo", '*'), r8)

    val r9 = sl.updateWith[Int](i => (i + 1).toString)
    assertEquals(("2", true, "foo", 2.0), r9)

    val r10 = sl.updateWith[Boolean](b => if(b) 3.0 else 2.0)
    assertEquals((1, 3.0, "foo", 2.0), r10)

    val r11 = sl.updateWith[String](s => s.length)
    assertEquals((1, true, 3, 2.0), r11)

    val r12 = sl.updateWith[Double](d => '*')
    assertEquals((1, true, "foo", '*'), r12)

    val fruits = (a, p, a, f)

    val rr1 = fruits.updatedType[Pear](a)
    typed[(Apple, Apple, Apple, Fruit)](rr1)

    val rr2 = fruits.updatedType[Pear](f)
    typed[(Apple, Fruit, Apple, Fruit)](rr2)

    val rr3 = fruits.updatedType[Fruit](p)
    typed[(Apple, Pear, Apple, Pear)](rr3)

    val rr4 = fruits.updatedElem(p)
    typed[(Apple, Pear, Apple, Fruit)](rr4)

    val rr5 = fruits.updatedElem(f)
    typed[(Apple, Pear, Apple, Fruit)](rr5)

    val rr6 = fruits.updateWith[Pear](p => a)
    typed[(Apple, Apple, Apple, Fruit)](rr6)

    val rr7 = fruits.updateWith[Fruit](f => p)
    typed[(Apple, Pear, Apple, Pear)](rr7)

    val rr8 = fruits.updateWith[Pear](p => f)
    typed[(Apple, Fruit, Apple, Fruit)](rr8)
  }

  @Test
  def testSplitLeft {
    val sl = (1, true, "foo", 2.0)
    val sl2 = (23, 3.0, "foo", (), "bar", true, 5L)

    val (sp1, sp2) = sl.splitLeft[String]
    typed[(Int, Boolean)](sp1)
    typed[(String, Double)](sp2)
    assertEquals((sp1 ::: sp2), sl)

    val (sli1, sli2) = sl2.splitLeft[String]
    typed[(Int, Double)](sli1)
    typed[(String, Unit, String, Boolean, Long)](sli2)
    assertEquals((sli1 ::: sli2), sl2)

    val (rsp1, rsp2) = sl.reverse_splitLeft[String]
    typed[(Boolean, Int)](rsp1)
    typed[(String, Double)](rsp2)
    assertEquals((rsp1 reverse_::: rsp2), sl)

    val (rsli1, rsli2) = sl2.reverse_splitLeft[String]
    typed[(Double, Int)](rsli1)
    typed[(String, Unit, String, Boolean, Long)](rsli2)
    assertEquals((rsli1 reverse_::: rsli2), sl2)

  }

  @Test
  def testSplitRight {
    val sl = (1, true, "foo", 2.0)
    val sl2 = (23, 3.0, "foo", (), "bar", true, 5L)

    val (srp1, srp2) = sl.splitRight[String]
    typed[(Int, Boolean, String)](srp1)
    typed[Tuple1[Double]](srp2)
    assertEquals((srp1 ::: srp2), sl)

    val (srli1, srli2) = sl2.splitRight[String]
    typed[(Int, Double, String, Unit, String)](srli1)
    typed[(Boolean, Long)](srli2)
    assertEquals(sl2, srli1 ::: srli2)

    val (rsrp1, rsrp2) = sl.reverse_splitRight[String]
    typed[(String, Boolean, Int)](rsrp1)
    typed[Tuple1[Double]](rsrp2)
    assertEquals((rsrp1 reverse_::: rsrp2), sl)

    val (rsrli1, rsrli2) = sl2.reverse_splitRight[String]
    typed[(String, Unit, String, Double, Int)](rsrli1)
    typed[(Boolean, Long)](rsrli2)
    assertEquals((rsrli1 reverse_::: rsrli2), sl2)
  }

  @Test
  def testTranspose {
    val l1 = Tuple1(1)
    val l2 = Tuple1(Tuple1("a"))

    val z1 = l1.zipOne(l2)
    typed[Tuple1[(Int, String)]](z1)
    assertEquals(Tuple1((1, "a")), z1)

    val mc1 = l1.mapConst(())
    typed[Tuple1[Unit]](mc1)
    assertEquals(Tuple1(()), mc1)

    val t1 = Tuple1(l1).transpose
    typed[Tuple1[Tuple1[Int]]](t1)
    assertEquals(Tuple1(Tuple1(1)), t1)

    val l3 = (1, 2, 3)
    val l4 = (("a", 1.0), ("b", 2.0), ("c", 3.0))

    val z2 = l3.zipOne(l4)
    typed[((Int, String, Double), (Int, String, Double), (Int, String, Double))](z2)
    assertEquals(((1, "a", 1.0), (2, "b", 2.0), (3, "c", 3.0)), z2)

    val mc2 = l3.mapConst(())
    typed[(Unit, Unit, Unit)](mc2)
    assertEquals(((), (), ()), mc2)

    val t2 = l4.transpose
    typed[((String, String, String), (Double, Double, Double))](t2)
    assertEquals((("a", "b", "c"), (1.0, 2.0, 3.0)), t2)

    val t3 = z2.transpose
    typed[((Int, Int, Int), (String, String, String), (Double, Double, Double))](t3)
    assertEquals(((1, 2, 3), ("a", "b", "c"), (1.0, 2.0, 3.0)), t3)

    val t4 = t3.transpose
    typed[((Int, String, Double), (Int, String, Double), (Int, String, Double))](t4)
    assertEquals(z2, t4)
  }

  @Test
  def testZipUnzip {
    val l1 = (1, "a", 1.0)
    val l2 = (2, "b", 2.0)

    val z1 = (l1, l2).transpose
    typed[((Int, Int), (String, String), (Double, Double))](z1)
    assertEquals(((1, 2), ("a", "b"), (1.0, 2.0)), z1)

    val z2 = l1 zip l2
    typed[((Int, Int), (String, String), (Double, Double))](z2)
    assertEquals(((1, 2), ("a", "b"), (1.0, 2.0)), z2)

    val z3 = (l1, l2).zip
    typed[((Int, Int), (String, String), (Double, Double))](z3)
    assertEquals(((1, 2), ("a", "b"), (1.0, 2.0)), z3)

    val u1 = z1.transpose
    typed[((Int, String, Double), (Int, String, Double))](u1)
    assertEquals(((1, "a", 1.0), (2, "b", 2.0)), u1)

    val u2 = z1.unzip
    typed[((Int, String, Double), (Int, String, Double))](u2)
    assertEquals(((1, "a", 1.0), (2, "b", 2.0)), u2)

    val intInc : Int => Int = _+1
    val stringInc : String => String = _+"*"
    val doubleInc : Double => Int = _.toInt+1

    val l3 = (intInc, stringInc, doubleInc)

    val z5 = l3 zipApply l1
    typed[(Int, String, Int)](z5)
    assertEquals((2, "a*", 2), z5)
  }

  @Test
  def testRemove {
    val l = (1, true, "foo")

    val li = l.removeElem[Int]
    typed[(Int, (Boolean, String))](li)
    assertEquals((1, (true, "foo")), li)

    val lb = l.removeElem[Boolean]
    typed[(Boolean, (Int, String))](lb)
    assertEquals((true, (1, "foo")), lb)

    val ls = l.removeElem[String]
    typed[(String, (Int, Boolean))](ls)
    assertEquals(("foo", (1, true)), ls)
  }

  @Test
  def testRemoveAll {
    val l = (1, true, "foo")

    val lnil = l.removeAll[Unit]
    typed[(Unit, (Int, Boolean, String))](lnil)
    assertEquals(((), (1, true, "foo")), lnil)

    val li = l.removeAll[Tuple1[Int]]
    typed[(Tuple1[Int], (Boolean, String))](li)
    assertEquals((Tuple1(1), (true, "foo")), li)

    val lb = l.removeAll[Tuple1[Boolean]]
    typed[(Tuple1[Boolean], (Int, String))](lb)
    assertEquals((Tuple1(true), (1, "foo")), lb)

    val lbi = l.removeAll[(Boolean, Int)]
    typed[((Boolean, Int), Tuple1[String])](lbi)
    assertEquals(((true, 1), Tuple1("foo")), lbi)
  }

  object combine extends Poly {
    implicit def caseCharString = use((c : Char, s : String) => s.indexOf(c))
    implicit def caseIntBoolean = use((i : Int, b : Boolean) => if ((i >= 0) == b) "pass" else "fail")
  }

  @Test
  def testFoldLeft {
    val c1a = combine('o', "foo")
    val c1b = combine(c1a, true)
    assertEquals("pass", c1b)

    val l1 = ("foo", true)
    val f1 = l1.foldLeft('o')(combine)
    typed[String](f1)
    assertEquals("pass", f1)

    val c2a = combine('o', "bar")
    val c2b = combine(c2a, false)
    assertEquals("pass", c2b)

    val l2 = ("bar", false)
    val f2 = l2.foldLeft('o')(combine)
    typed[String](f2)
    assertEquals("pass", f2)
  }

  @Test
  def testUpdatedAt {
    type IBS = (Int, Boolean, String)

    val l = (1, true, "foo")

    val li = l.updatedAt[_0](2)
    typed[IBS](li)
    assertEquals((2, true, "foo"), li)

    val lb = l.updatedAt[_1](false)
    typed[IBS](lb)
    assertEquals((1, false, "foo"), lb)

    val ls = l.updatedAt[_2]("bar")
    typed[IBS](ls)
    assertEquals((1, true, "bar"), ls)
  }

  @Test
  def testUpdatedAtLiteral {
    type IBS = (Int, Boolean, String)

    val l = (1, true, "foo")

    val li = l.updatedAt(0, 2)
    typed[IBS](li)
    assertEquals((2, true, "foo"), li)

    val lb = l.updatedAt(1, false)
    typed[IBS](lb)
    assertEquals((1, false, "foo"), lb)

    val ls = l.updatedAt(2, "bar")
    typed[IBS](ls)
    assertEquals((1, true, "bar"), ls)
  }

  @Test
  def testZipConst {
    val l1 = (1, true, "a")
    val c1 = 5
    val zl1 = l1 zipConst c1
    typed[((Int, Int), (Boolean, Int), (String, Int))](zl1)
    assertEquals(((1, c1), (true, c1), ("a", c1)), zl1)

    val l2 = (Option("a"), 2, Set(true))
    val c2 = ("b", 5)
    type C2 = (String, Int)
    val zl2 = l2 zipConst c2
    typed[(
      (Option[String], C2),
      (Int,            C2),
      (Set[Boolean],   C2))](zl2)
    val expected = (
      (Option("a"), c2),
      (2, c2),
      (Set(true), c2))
    assertEquals(expected, zl2)
  }

  @Test
  def testZipWithIndex {

    // Unit zipWithIndex
    val l1 = ()
    val zl1 = ().zipWithIndex
    typed[Unit](zl1)
    assertEquals((), zl1)

    // Tuple zipWithIndex
    val l2 = (1, true, "a")
    val zl2 = l2.zipWithIndex
    typed[((Int, _0), (Boolean, _1), (String, _2))](zl2)
    assertEquals(((1, _0), (true, _1), ("a", _2)), zl2)

  }


  @Test
  def testPropagation {
    def useHead[P <: Product](p: P)(implicit ic: ops.tuple.IsComposite[P]) = p.head

    val h = useHead((23, "foo", true))
    typed[Int](h)
  }

  @Test
  def testCollect {
    import poly._

    object empty extends Poly1

    object complex extends Poly1 {
      implicit val caseInt    = at[Int](_.toDouble)
      implicit val caseString = at[String](_ => 1)
    }

    { // () collect p
      val in: Unit = ()

      val emptyResult = in.collect(empty)
      typed[Unit](emptyResult)
      assertEquals((), emptyResult)

      val identityResult = in.collect(poly.identity)
      typed[Unit](identityResult)
      assertEquals((), identityResult)

      val complexResult = in.collect(complex)
      typed[Unit](complexResult)
      assertEquals((), complexResult)
    }

    { // non-() collect empty
      val in: (Int, String, Double) = (1, "foo", 2.2)

      val result = in.collect(empty)
      typed[Unit](result)
      assertEquals((), result)
    }

    { // t collect identity
      val in: (Int, String, Double) = (1, "foo", 2.2)

      val result = in.collect(identity)
      typed[(Int, String, Double)](result)
      assertEquals(in, result)
    }

    { // t collect complex
      val in: (Int, String, Double) = (1, "foo", 2.2)

      val result = in.collect(complex)
      typed[(Double, Int)](result)
      assertEquals((1.0, 1), result)
    }
  }

  @Test
  def testPermutations {
    assertEquals(((1, "foo"), ("foo", 1)), (1, "foo").permutations)

    assertEquals((
      (1, "foo", 2.0), ("foo", 1, 2.0), ("foo", 2.0, 1),
      (1, 2.0, "foo"), (2.0, 1, "foo"), (2.0, "foo", 1)
    ), (1, "foo", 2.0).permutations)
  }

  @Test
  def testMkString {
    assertEquals("<1;foo;2.0>", (1, "foo", 2.0).mkString("<", ";", ">"))
  }

  @Test
  def testRotateLeft {
    val in2 = (1, "foo")
    val in3 = (1, "foo", 2.0)
    val in4 = (1, "foo", 2.0, 'a')
    type S = String; type I = Int; type D = Double; type C = Char

    // rotateLeft(0)
    val r1 = in2.rotateLeft(0)
    assertTypedEquals[(I, S)](in2, r1)
    val r2 = in3.rotateLeft(0)
    assertTypedEquals[(I, S, D)](in3, r2)
    val r3 = in4.rotateLeft(0)
    assertTypedEquals[(I, S, D, C)](in4, r3)

    // rotateLeft(N % Size == 0)
    val r4 = in2.rotateLeft(2)
    assertTypedEquals[(I, S)](in2, r4)
    val r5 = in2.rotateLeft(4)
    assertTypedEquals[(I, S)](in2, r5)
    val r6 = in3.rotateLeft(3)
    assertTypedEquals[(I, S, D)](in3, r6)
    val r7 = in3.rotateLeft(6)
    assertTypedEquals[(I, S, D)](in3, r7)
    val r8 = in4.rotateLeft(4)
    assertTypedEquals[(I, S, D, C)](in4, r8)
    val r9 = in4.rotateLeft(8)
    assertTypedEquals[(I, S, D, C)](in4, r9)

    // other
    val r10 = in2.rotateLeft(1)
    assertTypedEquals[(S, I)](("foo", 1), r10)
    val r11 = in3.rotateLeft(1)
    assertTypedEquals[(S, D, I)](("foo", 2.0, 1), r11)
    val r12 = in4.rotateLeft(1)
    assertTypedEquals[(S, D, C, I)](("foo", 2.0, 'a', 1), r12)
    val r13 =  in4.rotateLeft(2)
    assertTypedEquals[(D, C, I, S)]((2.0, 'a', 1, "foo"), r13)
    val r14 = in4.rotateLeft(3)
    assertTypedEquals[(C, I, S, D)](('a', 1, "foo", 2.0), r14)
    val r15 = in4.rotateLeft(5)
    assertTypedEquals[(S, D, C, I)](("foo", 2.0, 'a', 1), r15)
    val r16 = in4.rotateLeft(6)
    assertTypedEquals[(D, C, I, S)]((2.0, 'a', 1, "foo"), r16)
  }

  @Test
  def testRotateRight {
    val in2 = (1, "foo")
    val in3 = (1, "foo", 2.0)
    val in4 = (1, "foo", 2.0, 'a')
    type S = String; type I = Int; type D = Double; type C = Char

    // rotateRight(0)
    val r1 = in2.rotateRight(0)
    assertTypedEquals[(I, S)](in2, r1)
    val r2 = in3.rotateRight(0)
    assertTypedEquals[(I, S, D)](in3, r2)
    val r3 = in4.rotateRight(0)
    assertTypedEquals[(I, S, D, C)](in4, r3)

    // rotateRight(N % Size == 0)
    val r4 = in2.rotateRight(2)
    assertTypedEquals[(I, S)](in2, r4)
    val r5 = in2.rotateRight(4)
    assertTypedEquals[(I, S)](in2, r5)
    val r6 = in3.rotateRight(3)
    assertTypedEquals[(I, S, D)](in3, r6)
    val r7 = in3.rotateRight(6)
    assertTypedEquals[(I, S, D)](in3, r7)
    val r8 = in4.rotateRight(4)
    assertTypedEquals[(I, S, D, C)](in4, r8)
    val r9 = in4.rotateRight(8)
    assertTypedEquals[(I, S, D, C)](in4, r9)

    // others
    val r10 = in2.rotateRight(1)
    assertTypedEquals[(S, I)](("foo", 1), r10)
    val r11 = in3.rotateRight(1)
    assertTypedEquals[(D, I, S)]((2.0, 1, "foo"), r11)
    val r12 = in4.rotateRight(1)
    assertTypedEquals[(C, I, S, D)](('a', 1, "foo", 2.0), r12)
    val r13 = in4.rotateRight(2)
    assertTypedEquals[(D, C, I, S)]((2.0, 'a', 1, "foo"), r13)
    val r14 = in4.rotateRight(3)
    assertTypedEquals[(S, D, C, I)](("foo", 2.0, 'a', 1), r14)
    val r15 = in4.rotateRight(5)
    assertTypedEquals[(C, I, S, D)](('a', 1, "foo", 2.0), r15)
    val r16 = in4.rotateRight(6)
    assertTypedEquals[(D, C, I, S)]((2.0, 'a', 1, "foo"), r16)
  }

  object smear extends Poly {
    implicit val caseIntInt    = use((x: Int, y: Int) => x + y)
    implicit val caseStringInt = use((x: String, y: Int) => x.toInt + y)
    implicit val caseIntString = use((x: Int, y: String) => x + y.toInt)
  }

  @Test
  def testScanLeft {
    val in = (1, "2", 3)
    val out = in.scanLeft(1)(smear)

    typed[(Int, Int, Int, Int)](out)
    assertEquals((1, 2, 4, 7), out)
  }

  @Test
  def testScanRight {
    val in = (1, "2", 3)
    val out = in.scanRight(1)(smear)

    typed[(Int, Int, Int, Int)](out)
    assertEquals((7, 6, 4, 1), out)
  }

  @Test
  def testFill {
    {
      val empty = Tuple.fill(0)(true)
      typed[Unit](empty)
    }

    {
      val empty = Tuple.fill[Boolean](0)(true)
      typed[Unit](empty)
    }

    {
      val single = Tuple.fill(1)(None)
      typed[Tuple1[None.type]](single)
      assertEquals(Tuple1(None), single)
    }

    {
      val single = Tuple.fill[None.type](1)(None)
      typed[Tuple1[None.type]](single)
      assertEquals(Tuple1(None), single)
    }

    {
      val three = Tuple.fill(3)(m2i)
      typed[(M2[Int, Unit], M2[Int, Unit], M2[Int, Unit])](three)
      assertEquals((m2i, m2i, m2i), three)
    }

    {
      val three = Tuple.fill[M2[Int, Unit]](3)(m2i)
      typed[(M2[Int, Unit], M2[Int, Unit], M2[Int, Unit])](three)
      assertEquals((m2i, m2i, m2i), three)
    }

    {
      val empty = Tuple.fill(0, 0)(true)
      typed[Unit](empty)
    }

    {
      val empty = Tuple.fill[Boolean](0, 0)(true)
      typed[Unit](empty)
    }

    {
      val empty = Tuple.fill(2, 0)(true)
      typed[(Unit, Unit)](empty)
    }

    {
      val empty = Tuple.fill[Boolean](2, 0)(true)
      typed[(Unit, Unit)](empty)
    }

    {
      val empty = Tuple.fill(0, 2)(true)
      typed[Unit](empty)
    }

    {
      val empty = Tuple.fill[Boolean](0, 2)(true)
      typed[Unit](empty)
    }

    {
      val oneByTwo = Tuple.fill(1, 2)(None)
      typed[Tuple1[(None.type, None.type)]](oneByTwo)
      assertEquals(Tuple1((None, None)), oneByTwo)
    }

    {
      val oneByTwo = Tuple.fill[None.type](1, 2)(None)
      typed[Tuple1[(None.type, None.type)]](oneByTwo)
      assertEquals(Tuple1((None, None)), oneByTwo)
    }

    {
      val twoByThree = Tuple.fill(2, 3)(None)
      typed[((None.type, None.type, None.type), (None.type, None.type, None.type))](twoByThree)
      assertEquals(((None, None, None), (None, None, None)), twoByThree)
    }

    {
      val twoByThree = Tuple.fill[None.type](2, 3)(None)
      typed[((None.type, None.type, None.type), (None.type, None.type, None.type))](twoByThree)
      assertEquals(((None, None, None), (None, None, None)), twoByThree)
    }
  }

  @Test
  def testPatch{
    val in = (1, "two", 3)

    { //single patch w/ nothing removed
      val out = in.patch(1, (4,5), 0)
      val out2 = in.patch[_1, _0]((4,5))

      typed[(Int, Int, Int, String, Int)](out)
      assertEquals((1, 4, 5, "two", 3), out)
      assertTypedEquals[(Int, Int, Int, String, Int)](out, out2)
    }

    { //single patch w/ 2 elements removed
      val out = in.patch(1, (3, 4), 2)
      val out2 = in.patch[_1,_2]((3,4))

      typed[(Int, Int, Int)](out)
      assertEquals((1, 3, 4), out)
      assertTypedEquals[(Int, Int, Int)](out, out2)
    }

    { //essentially append
      val out = in.patch(3, (4, 5, "six"), 0)
      val out2 = in.patch[_3,_0]((4, 5, "six"))

      typed[(Int, String, Int, Int, Int, String)](out)
      assertEquals((1, "two", 3, 4, 5, "six"), out)
      assertTypedEquals[(Int, String, Int, Int, Int, String)](out, out2)
    }

    { //several patched w/ everything from original removed
      val sub = (4, "five", "six")
      val out = in.patch(0, sub, 3)
      val out2 = in.patch[_0,_3]((4, "five", "six"))

      typed[(Int, String, String)](out)
      assertEquals(sub, out)
      assertTypedEquals[(Int, String, String)](out, out2)
    }
  }

  @Test
  def testGrouper {
    object toInt extends Poly1 {
      implicit def default[N <: Nat](implicit toi: ops.nat.ToInt[N]) = at[N](_ => toi())
    }
    def range[R <: HList, T, OutL <: HList](a: Nat, b: Nat)(implicit
                                                            range: ops.nat.Range.Aux[a.N, b.N, R],
                                                            mapper: ops.hlist.Mapper.Aux[toInt.type, R, OutL],
                                                            tupler: ops.hlist.Tupler.Aux[OutL, T]
      ) = tupler(mapper(range()))

    // group Unit
    assertEquals( HNil : HNil, (HNil: HNil) group (2,1) )

    // partition a Tuple of 20 items into 5 (20/4) tuples of 4 items
    assertEquals(
      ((0, 1, 2, 3), (4, 5, 6, 7), (8, 9, 10, 11), (12, 13, 14, 15), (16, 17, 18, 19)),
      range(0,20) group (4, 4)
    )

    // partition a Tuple of 22 items into 5 (20/4) tuples of 4 items
    // the last two items do not make a complete partition and are dropped.
    assertEquals(
      ((0, 1, 2, 3), (4, 5, 6, 7), (8, 9, 10, 11), (12, 13, 14, 15), (16, 17, 18, 19)),
      range(0, 22) group (4, 4)
    )

    // uses the step to select the starting point for each partition
    assertEquals(
      ((0, 1, 2, 3), (6, 7, 8, 9), (12, 13, 14, 15)),
      range(0, 20) group (4, 6)
    )

    // if the step is smaller than the partition size, items will be reused
    assertEquals(
      ((0, 1, 2, 3), (3, 4, 5, 6), (6, 7, 8, 9), (9, 10, 11, 12), (12, 13, 14, 15), (15, 16, 17, 18)),
      range(0, 20) group (4, 3)
    )

    // when there are not enough items to fill the last partition, a pad can be supplied.
    assertEquals(
      ((0, 1, 2), (6, 7, 8), (12, 13, 14), (18, 19, 'a')),
      range(0, 20) group (3, 6, Tuple1('a'))
    )

    // but only as many pad elements are used as necessary to fill the final partition.
    assertEquals(
      ((0, 1, 2, 3), (6, 7, 8, 9), (12, 13, 14, 15), (18, 19, 'a', 'b')),
      range(0, 20) group (4, 6, ('a', 'b', 'c', 'd', 'e', 'f', 'g'))
    )

  }
}
