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

  object mkString extends (Any -> String)(_.toString)
  object fruit extends (Fruit -> Fruit)(f => f)
  object incInt extends (Int >-> Int)(_ + 1)
  object extendedChoose extends LiftU(choose)
  
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
    // arguments fails, presumably due to a failure to compute a sensble LUB.
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
  def testToList {
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
  }
  
  @Test
  def testToArray {
    def assertArrayEquals2[T](arr1 : Array[T], arr2 : Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr1.asInstanceOf[Array[Object]])
    
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
  def testPropagation {
    def useHead[P <: Product](p: P)(implicit ic: ops.tuple.IsComposite[P]) = p.head

    val h = useHead((23, "foo", true))
    typed[Int](h)
  }
}
