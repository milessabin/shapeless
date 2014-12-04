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

import test._
import testutil._

class HListTests {
  import nat._
  import poly._
  import syntax.std.traversable._
  import syntax.typeable._
  import ops.hlist._

  type SI = Set[Int] :: HNil
  type OI = Option[Int] :: HNil

  type SISS = Set[Int] :: Set[String] :: HNil
  type OIOS = Option[Int] :: Option[String] :: HNil

  type ISII = Int :: String :: Int :: Int :: HNil
  type IIII = Int :: Int :: Int :: Int :: HNil
  type IYII = Int :: Any :: Int :: Int :: HNil

  type OIOSOIOI = Option[Int] :: Option[String] :: Option[Int] :: Option[Int] :: HNil
  type SISSSISI = Set[Int] :: Set[String] :: Set[Int] :: Set[Int] :: HNil

  type BBBB = Boolean :: Boolean :: Boolean :: Boolean :: HNil

  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit

  type PWS = Product with Serializable with Fruit

  type YYYY = Any :: Any :: Any :: Any :: HNil
  type FF = Fruit :: Fruit :: HNil
  type AP = Apple :: Pear :: HNil
  type BP = Banana :: Pear :: HNil
  type AF = Apple :: Fruit :: HNil
  type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
  type APAP = Apple :: Pear :: Apple :: Pear :: HNil
  type APBP = Apple :: Pear :: Banana :: Pear :: HNil
  type APB = Apple :: Pear :: Banana :: HNil
  type PBPA = Pear :: Banana :: Pear :: Apple :: HNil
  type PABP = Pear :: Apple :: Banana :: Pear :: HNil

  val a : Apple = Apple()
  val p : Pear = Pear()
  val b : Banana = Banana()
  val f : Fruit = new Fruit {}

  val ap : AP = a :: p :: HNil
  val bp : BP = b :: p :: HNil
  val apap : APAP = a :: p :: a :: p :: HNil
  val apbp : APBP = a :: p :: b :: p :: HNil
  val apapList = a :: p :: a :: p :: Nil
  val apbpList = a :: p :: b :: p :: Nil
  val apapArray = Array(a, p, a, p)
  val apbpArray = Array(a, p, b, p)

  trait Ctv[-T]
  type CICSCICICD = Ctv[Int] :: Ctv[String] :: Ctv[Int] :: Ctv[Int] :: Ctv[Double] :: HNil

  val ci: Ctv[Int] = new Ctv[Int] {}
  val cs: Ctv[String] = new Ctv[String] {}
  val cd: Ctv[Double] = new Ctv[Double] {}
  val cicscicicdList = ci :: cs :: ci :: ci :: cd :: Nil
  val cicscicicdArray = Array(ci, cs, ci, ci, cd)
  val cicscicicd: CICSCICICD = ci :: cs :: ci :: ci :: cd :: HNil

  trait M[T]
  type MIMSMIMIMD = M[Int] :: M[String] :: M[Int] :: M[Int] :: M[Double] :: HNil

  val mi: M[Int] = new M[Int] {}
  val ms: M[String] = new M[String] {}
  val md: M[Double] = new M[Double] {}
  val mimsmimimdList = mi :: ms :: mi :: mi :: md :: Nil
  val mimsmimimdArray = Array(mi, ms, mi, mi, md)
  val mimsmimimd: MIMSMIMIMD = mi :: ms :: mi :: mi :: md :: HNil

  import language.existentials
  val mExist: M[_] = new M[Double] {}
  type MIMSMIMEMD = M[Int] :: M[String] :: M[Int] :: M[_] :: M[Double] :: HNil
  val mimsmimemdList = mi :: ms :: mi :: mExist :: md :: Nil
  val mimsmimemdArray = Array[M[_]](mi, ms, mi, mExist, md)
  val mimsmimemd: MIMSMIMEMD = mi :: ms :: mi :: mExist :: md :: HNil

  trait M2[A,B]
  type M2IM2SM2IM2IM2D = M2[Int, Unit] :: M2[String, Unit] :: M2[Int, Unit] :: M2[Int, Unit] :: M2[Double, Unit] :: HNil

  val m2i: M2[Int, Unit] = new M2[Int, Unit] {}
  val m2s: M2[String, Unit] = new M2[String, Unit] {}
  val m2d: M2[Double, Unit] = new M2[Double, Unit] {}
  val m2im2sm2im2im2dList = m2i :: m2s :: m2i :: m2i :: m2d :: Nil
  val m2im2sm2im2im2dArray = Array(m2i, m2s, m2i, m2i, m2d)
  val m2im2sm2im2im2d: M2IM2SM2IM2IM2D = m2i :: m2s :: m2i :: m2i :: m2d :: HNil

  val m2iExist: M2[Int, _] = new M2[Int, Unit] {}
  val m2sExist: M2[String, _] = new M2[String, Unit] {}
  val m2dExist: M2[Double, _] = new M2[Double, Unit] {}
  type M2EIM2ESM2EIM2EEM2ED = M2[Int, _] :: M2[String, _] :: M2[Int, _] :: M2[Int, _] :: M2[Double, _] :: HNil
  val m2eim2esm2eim2eem2edList = m2iExist :: m2sExist :: m2iExist :: m2iExist :: m2dExist :: Nil
  val m2eim2esm2eim2eem2edArray = Array(m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)
  val m2eim2esm2eim2eem2ed: M2EIM2ESM2EIM2EEM2ED = m2iExist :: m2sExist :: m2iExist :: m2iExist :: m2dExist :: HNil

  object mkString extends (Any -> String)(_.toString)
  object fruit extends (Fruit -> Fruit)(f => f)
  object incInt extends (Int >-> Int)(_ + 1)
  object extendedChoose extends LiftU(choose)

  @Test
  def testBasics {
    val l = 1 :: "foo" :: 2.0 :: HNil

    val r1 = l.head
    assertTypedEquals[Int](1, r1)

    val r2 = l.tail.head
    assertTypedEquals[String]("foo", r2)

    assertEquals(2.0, l.tail.tail.head, Double.MinPositiveValue)

    illTyped("""
      HNil.head
    """)

    illTyped("""
      HNil.tail
    """)

    illTyped("""
      l.tail.tail.tail.head
    """)
  }

  @Test
  def testMap {
    implicitly[Mapper.Aux[choose.type, HNil, HNil]]
    implicitly[choose.Case[Set[Int]]]
    implicitly[Mapper.Aux[choose.type, Set[Int] :: HNil, Option[Int] :: HNil]]

    val s1 = Set(1) :: HNil
    val o1 = s1 map choose
    assertTypedEquals[OI](Option(1) :: HNil, o1)

    val s2 = Set(1) :: Set("foo") :: HNil
    val o2 = s2 map choose
    assertTypedEquals[OIOS](Option(1) :: Option("foo") :: HNil, o2)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val l2 = l1 map singleton
    assertTypedEquals[SISSSISI](Set(1) :: Set("foo") :: Set(2) :: Set(3) :: HNil, l2)

    val l3 = l1 map option
    assertTypedEquals[OIOSOIOI](Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil, l3)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil

    val l5 = l4 map get
    assertTypedEquals[ISII](1 :: "foo" :: 2 :: 3 :: HNil, l5)

    typed[Int](l5.head)
    typed[String](l5.tail.head)
    typed[Int](l5.tail.tail.head)
    typed[Int](l5.tail.tail.tail.head)

    val l6 = l1 map identity
    assertTypedEquals[ISII](1 :: "foo" :: 2 :: 3 :: HNil, l6)

    val l7 = l4 map isDefined
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val l8 = 23 :: "foo" :: true :: HNil
    val l9 = l8 map mkString
    assertTypedEquals[String :: String :: String :: HNil]("23" :: "foo" :: "true" :: HNil, l9)

    val l10 = apbp map fruit
    assertTypedEquals[Fruit :: Fruit :: Fruit :: Fruit :: HNil](apbp, l10)

    val l11 = apbp map mkString
    assertTypedEquals[String :: String :: String :: String :: HNil]("Apple()" :: "Pear()" :: "Banana()" :: "Pear()" :: HNil, l11)
  }

  object dup extends Poly1 {
    implicit def default[T] = at[T](t => t :: t :: HNil)
  }

  @Test
  def testFlatMap {
    val l1 = 1 :: "foo" :: true :: HNil

    val l2 = l1 flatMap dup
    assertTypedEquals[Int :: Int :: String :: String :: Boolean :: Boolean :: HNil](
      1 :: 1 :: "foo" :: "foo" :: true :: true :: HNil, l2)

    val l3 = (1 :: "foo" :: HNil) :: (HNil : HNil) :: (2.0 :: true :: HNil) :: ("bar" :: HNil) :: HNil

    val l4 = l3 flatMap identity
    assertTypedEquals[Int :: String :: Double :: Boolean :: String :: HNil](
      1 :: "foo" :: 2.0 :: true :: "bar" :: HNil, l4)

    val l5 = 23 :: "foo" :: 7 :: true :: 0 :: HNil
    val l6 = l5 flatMap incInt
    assertTypedEquals[Int :: Int :: Int :: HNil](24 :: 8 :: 1 :: HNil, l6)

    val l7 = Set(23) :: "foo" :: Set(true) :: 23 :: HNil
    val l8 = l7 flatMap extendedChoose
    assertTypedEquals[Option[Int] :: Option[Boolean] :: HNil](Option(23) :: Option(true) :: HNil, l8)
  }

  @Test
  def testConformance {
    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    assertTypedEquals[Any :: AnyRef :: Any :: Any :: HNil](1 :: "foo" :: 2 :: 3 :: HNil, l1)

    val ap = a :: p :: HNil
    typed[AP](ap)
    val bp = b :: p :: HNil
    typed[BP](bp)
    val apap = a :: p :: a :: p :: HNil
    typed[APAP](apap)
    val apbp = a :: p :: b :: p :: HNil
    typed[APBP](apbp)
    val ffff : FFFF = apap
    typed[FFFF](ffff)
  }

  @Test
  def testLength {
    val l0 = HNil
    typed[Nat._0](l0.length)
    assertEquals(0, Nat toInt l0.length)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    typed[Nat._4](l1.length)
    assertEquals(4, Nat toInt l1.length)

    val ap = a :: p :: HNil
    typed[Nat._2](ap.length)
    assertEquals(2, Nat toInt ap.length)

    val bp = b :: p :: HNil
    typed[Nat._2](bp.length)
    assertEquals(2, Nat toInt bp.length)

    val apap = a :: p :: a :: p :: HNil
    typed[Nat._4](apap.length)
    assertEquals(4, Nat toInt apap.length)

    val apbp = a :: p :: b :: p :: HNil
    typed[Nat._4](apbp.length)
    assertEquals(4, Nat toInt apbp.length)

    val ffff : FFFF = apap
    typed[Nat._4](ffff.length)
    assertEquals(4, Nat toInt ffff.length)
  }

  @Test
  def testInitLast {

    val lp = apbp.last
    assertTypedEquals[Pear](p, lp)

    val iapb = apbp.init
    assertTypedEquals[APB](a :: p :: b :: HNil, iapb)
  }

  @Test
  def testAlign {
    type M0 = Int :: String :: Boolean :: HNil
    type M1 = Int :: Boolean :: String :: HNil
    type M2 = String :: Int :: Boolean :: HNil
    type M3 = String :: Boolean :: Int :: HNil
    type M4 = Boolean :: Int :: String :: HNil
    type M5 = Boolean :: String :: Int :: HNil

    val m0 = 13 :: "bar" :: false :: HNil
    val m1 = 13 :: false :: "bar" :: HNil
    val m2 = "bar" :: 13 :: false :: HNil
    val m3 = "bar" :: false :: 13 :: HNil
    val m4 = false :: 13 :: "bar" :: HNil
    val m5 = false :: "bar" :: 13 :: HNil

    val l = 23 :: "foo" :: true :: HNil

    val a0 = l.align(m0)
    assertTypedEquals[M0](23 :: "foo" :: true :: HNil, a0)

    val a1 = l.align(m1)
    assertTypedEquals[M1](23 :: true :: "foo" :: HNil, a1)

    val a2 = l.align(m2)
    assertTypedEquals[M2]("foo" :: 23 :: true :: HNil, a2)

    val a3 = l.align(m3)
    assertTypedEquals[M3]("foo" :: true :: 23 :: HNil, a3)

    val a4 = l.align(m4)
    assertTypedEquals[M4](true :: 23 :: "foo" :: HNil, a4)

    val a5 = l.align(m5)
    assertTypedEquals[M5](true :: "foo" :: 23 :: HNil, a5)

    val b0 = l.align[M0]
    assertTypedEquals[M0](23 :: "foo" :: true :: HNil, b0)

    val b1 = l.align[M1]
    assertTypedEquals[M1](23 :: true :: "foo" :: HNil, b1)

    val b2 = l.align[M2]
    assertTypedEquals[M2]("foo" :: 23 :: true :: HNil, b2)

    val b3 = l.align[M3]
    assertTypedEquals[M3]("foo" :: true :: 23 :: HNil, b3)

    val b4 = l.align[M4]
    assertTypedEquals[M4](true :: 23 :: "foo" :: HNil, b4)

    val b5 = l.align[M5]
    assertTypedEquals[M5](true :: "foo" :: 23 :: HNil, b5)

    val c0 = (HNil: HNil).align[HNil]
    typed[HNil](c0)

    val c1 = (23 :: HNil).align[Int :: HNil]
    typed[Int :: HNil](c1)

    val c2 = (23 :: "foo" :: HNil).align[String :: Int :: HNil]
    typed[String :: Int :: HNil](c2)

    illTyped("""
      (HNil: HNil).align[Int :: HNil]
    """)

    illTyped("""
      (23 :: HNil).align[String :: HNil]
    """)

    illTyped("""
      (23 :: "foo" :: HNil).align[String :: String :: HNil]
    """)
  }

  @Test
  def testReverse {
    val pbpa = apbp.reverse
    assertTypedEquals[PBPA](p :: b :: p :: a :: HNil, pbpa)

    val al = a :: HNil
    val ral = al.reverse
    assertTypedEquals[Apple :: HNil](a :: HNil, ral)
  }

  @Test
  def testPrepend {
    val apbp2 = ap ::: bp
    assertTypedEquals[APBP](a :: p :: b :: p :: HNil, apbp2)

    typed[Apple](apbp2.head)
    typed[Pear](apbp2.tail.head)
    typed[Banana](apbp2.tail.tail.head)
    typed[Pear](apbp2.tail.tail.tail.head)

    val pabp = ap reverse_::: bp
    assertTypedEquals[PABP](p :: a :: b :: p :: HNil, pabp)

    // must compile without requiring an implicit Prepend
    def prependWithHNil[L <: HList](list: L) = HNil ::: list
    def prependToHNil[L <: HList](list: L) = list ::: HNil
    val r1 = prependWithHNil(ap)
    assertTypedEquals[AP](ap, r1)
    val r2 = prependToHNil(ap)
    assertTypedEquals[AP](ap, r2)
    val r3 = HNil ::: HNil
    assertTypedEquals[HNil](HNil, r3)

    // must compile without requiring an implicit ReversePrepend
    def reversePrependWithHNil[L <: HList](list: L) = HNil reverse_::: list
    def reversePrependToHNil[L <: HList: Reverse](list: L) = list reverse_::: HNil
    val r4 = reversePrependWithHNil(ap)
    assertTypedEquals[AP](ap, r4)
    val r5 = reversePrependToHNil(ap)
    assertTypedEquals[Pear :: Apple :: HNil](ap.reverse, r5)
    val r6 = HNil reverse_::: HNil
    assertTypedEquals[HNil](HNil, r6)
  }

  @Test
  def testToSizedList {
    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val hnil = HNil
    val snil = hnil.toSized[List]
    assertEquals(Nat toInt hnil.length, snil.length)
    val expectedUnsized = List.empty[Nothing]
    equalInferredTypes(expectedUnsized, snil.unsized)
    assertEquals(expectedUnsized, snil.unsized)

    implicitly[ToSized.Aux[HNil, List, Nothing, _0]]
    implicitly[ToSized.Aux[HNil, List, Int, _0]]

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
    assertTypedEquals[List[M2[_ >: Double with Int with String, _]]](
      m2eim2esm2eim2eem2edList, sizedM2eim2esm2eim2eem2ed.unsized)
  }

  @Test
  def testToSizedArray {
    def assertArrayEquals2[T](arr1 : Array[T], arr2 : Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr1.asInstanceOf[Array[Object]])

    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val hnil = HNil
    val snil = hnil.toSized[Array]
    assertEquals(Nat toInt hnil.length, snil.length)
    val expectedUnsized = Array.empty[Nothing]
    equalInferredTypes(expectedUnsized, snil.unsized)
    assertArrayEquals2(expectedUnsized, snil.unsized)

    implicitly[ToSized.Aux[HNil, Array, Nothing, _0]]
    implicitly[ToSized.Aux[HNil, Array, Int, _0]]

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
    typed[Array[M[_]]](sizedMimsmimemd.unsized)
    assertArrayEquals2(mimsmimemdArray, sizedMimsmimemd.unsized)

    val sizedM2im2sm2im2im2d = m2im2sm2im2im2d.toSized[Array]
    assertEquals(Nat toInt m2im2sm2im2im2d.length, sizedM2im2sm2im2im2d.length)
    equalInferredTypes(m2im2sm2im2im2dArray, sizedM2im2sm2im2im2d.unsized)
    assertArrayEquals2(m2im2sm2im2im2dArray, sizedM2im2sm2im2im2d.unsized)

    val sizedM2eim2esm2eim2eem2ed = m2eim2esm2eim2eem2ed.toSized[Array]
    assertEquals(Nat toInt m2eim2esm2eim2eem2ed.length, sizedM2eim2esm2eim2eem2ed.length)
    // equalInferredTypes(m2eim2esm2eim2eem2edArray, sizedM2eim2esm2eim2eem2ed.unsized)
    typed[Array[M2[_ >: Double with Int with String, _]]](sizedM2eim2esm2eim2eem2ed.unsized)
    assertArrayEquals2(m2eim2esm2eim2eem2edArray.map(x => x: Any), sizedM2eim2esm2eim2eem2ed.unsized.map(x => x: Any))
  }

  @Test
  def testUnifier {
    def lub[X, Y, L](x : X, y : Y)(implicit lb : Lub[X, Y, L]) : (L, L) = (lb.left(x), lb.right(y))

    val u21 = lub(a, a)
    typed[(Apple, Apple)](u21)
    val u22 = lub(a, p)
    typed[(Fruit, Fruit)](u22)
    val u23 = lub(a, f)
    typed[(Fruit, Fruit)](u23)
    val u24 = lub(p, a)
    typed[(Fruit, Fruit)](u24)
    val u25 = lub(p, p)
    typed[(Pear, Pear)](u25)
    val u26 = lub(f, f)
    typed[(Fruit, Fruit)](u26)
    val u27 = lub(f, a)
    typed[(Fruit, Fruit)](u27)
    val u28 = lub(f, p)
    typed[(Fruit, Fruit)](u28)
    val u29 = lub(f, f)
    typed[(Fruit, Fruit)](u29)

    implicitly[Lub[HNil, HNil, HNil]]
    implicitly[Lub[Apple :: HNil, Apple :: HNil, Apple :: HNil]]
    implicitly[Lub[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Lub[Apple :: Pear :: HNil, Pear :: Apple :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Lub[ISII, IIII, IYII]]

    val u31 = lub(HNil, HNil)
    typed[(HNil, HNil)](u31)
    val u32 = lub(a :: HNil, a :: HNil)
    typed[(Apple :: HNil, Apple :: HNil)](u32)
    val u33 = lub(f :: p :: HNil, f :: f :: HNil)
    typed[(Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil)](u33)
    val u34 = lub(a :: p :: HNil, p :: a :: HNil)
    typed[(Fruit :: Fruit :: HNil, Fruit :: Fruit :: HNil)](u34)
    val u35 = lub(1 :: "two" :: 3 :: 4 :: HNil, 1 :: 2 :: 3 :: 4 :: HNil)
    typed[(Int :: Any :: Int :: Int :: HNil, Int :: Any :: Int :: Int :: HNil)](u35)

    implicitly[Unifier.Aux[Apple :: HNil, Apple :: HNil]]
    implicitly[Unifier.Aux[Fruit :: Pear :: HNil, Fruit :: Fruit :: HNil]]
    implicitly[Unifier.Aux[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]]

    implicitly[Unifier.Aux[Int :: String :: Int :: Int :: HNil, YYYY]]

    val uapap = implicitly[Unifier.Aux[Apple :: Pear :: Apple :: Pear :: HNil, FFFF]]
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

    def getUnifier[L <: HList, Out <: HList](l : L)(implicit u : Unifier.Aux[L, Out]) = u

    val u2 = getUnifier(a :: HNil)
    typed[Unifier.Aux[Apple :: HNil, Apple :: HNil]](u2)
    val u3 = getUnifier(a :: a :: HNil)
    typed[Unifier.Aux[Apple :: Apple :: HNil, Apple :: Apple :: HNil]](u3)
    val u4 = getUnifier(a :: a :: a :: HNil)
    typed[Unifier.Aux[Apple :: Apple :: Apple :: HNil, Apple :: Apple :: Apple :: HNil]](u4)
    val u5 = getUnifier(a :: a :: a :: a :: HNil)
    typed[Unifier.Aux[Apple :: Apple :: Apple :: Apple :: HNil, Apple :: Apple :: Apple :: Apple :: HNil]](u5)
    val u6 = getUnifier(a :: p :: HNil)
    //typed[Unifier.Aux[Apple :: Pear :: HNil, Fruit :: Fruit :: HNil]](u6)
    val u7 = getUnifier(a :: f :: HNil)
    typed[Unifier.Aux[Apple :: Fruit :: HNil, Fruit :: Fruit :: HNil]](u7)
    val u8 = getUnifier(f :: a :: HNil)
    typed[Unifier.Aux[Fruit :: Apple :: HNil, Fruit :: Fruit :: HNil]](u8)
    val u9a = getUnifier(a :: f :: HNil)
    typed[Unifier.Aux[Apple :: Fruit :: HNil, FF]](u9a)
    val u9b = getUnifier(a :: p :: HNil)
    typed[Unifier.Aux[Apple :: Pear :: HNil, PWS :: PWS :: HNil]](u9b)
    val u10 = getUnifier(apap)
    typed[Unifier.Aux[APAP, PWS :: PWS :: PWS :: PWS :: HNil]](u10)
    val u11 = getUnifier(apbp)
    typed[Unifier.Aux[APBP, PWS :: PWS :: PWS :: PWS :: HNil]](u11)

    val invar1 = Set(23) :: Set("foo") :: HNil
    val uinvar1 = invar1.unify
    typed[Set[_ >: Int with String] :: Set[_ >: Int with String] :: HNil](uinvar1)

    // Unifying three or more elements which have an invariant outer type constructor and differing type
    // arguments fails, presumably due to a failure to compute a sensble LUB.
    //val invar2 = Set(23) :: Set("foo") :: Set(true) :: HNil
    //val uinvar2 = invar.unify
  }

  @Test
  def testSubtypeUnifier {
    val fruits : Apple :: Pear :: Fruit :: HNil = a :: p :: f :: HNil
    typed[Fruit :: Fruit :: Fruit :: HNil](fruits.unifySubtypes[Fruit])
    typed[Apple :: Pear :: Fruit :: HNil](fruits.unifySubtypes[Apple])
    assertEquals(a :: p :: f :: HNil, fruits.unifySubtypes[Fruit].filter[Fruit])

    val stuff : Apple :: String :: Pear :: HNil = a :: "foo" :: p :: HNil
    typed[Fruit :: String :: Fruit :: HNil](stuff.unifySubtypes[Fruit])
    assertEquals(HNil, stuff.filter[Fruit])
    assertEquals(a :: p :: HNil, stuff.unifySubtypes[Fruit].filter[Fruit])
  }

  @Test
  def testToTraversableList {
    val r1 = HNil.to[List]
    assertTypedEquals[List[Nothing]](Nil, r1)

    implicitly[ToList[HNil, Nothing]]
    implicitly[ToList[HNil, Int]]

    val r2 = apap.to[List]
    assertTypedEquals[List[Fruit]](List(a, p, a, p), r2)

    val fruits2 = apbp.to[List]
    assertTypedEquals[List[Fruit]](List(a, p, b, p), fruits2)

    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    assertTypedEquals[APBP](apbp, fruits3.get)

    val stuff = (1 :: "foo" :: 2 :: 3 :: HNil).to[List]
    assertTypedEquals[List[Any]](List(1, "foo", 2, 3), stuff)

    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    assertTypedEquals[ISII](1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4 map isDefined
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.to[List]
    typed[Boolean](ll2.head)

    val moreStuff = (a :: "foo" :: p :: HNil).to[List]
    typed[List[Any]](moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val ctv = cicscicicd.to[List]
    equalInferredTypes(cicscicicdList, ctv)
    assertTypedEquals[List[Ctv[Int with String with Double]]](cicscicicdList, ctv)

    val m = mimsmimimd.to[List]
    equalInferredTypes(mimsmimimdList, m)
    assertTypedEquals[List[M[_ >: Int with String with Double]]](mimsmimimdList, m)

    val mWithEx = mimsmimemd.to[List]
    //  equalType(mimsmimemdList, mWithEx)
    assertTypedEquals[List[M[_]]](mimsmimemdList, mWithEx)

    val m2 = m2im2sm2im2im2d.to[List]
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    assertTypedEquals[List[M2[_ >: Int with String with Double, Unit]]](m2im2sm2im2im2dList, m2)

    val m2e = m2eim2esm2eim2eem2ed.to[List]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    assertTypedEquals[List[M2[_ >: Int with String with Double, _]]](m2eim2esm2eim2eem2edList, m2e)
  }

  @Test
  def testToList {
    val r1 = HNil.toList
    assertTypedEquals[List[Nothing]](Nil, r1)

    implicitly[ToTraversable.Aux[HNil, List, Nothing]]
    implicitly[ToTraversable.Aux[HNil, List, Int]]

    val fruits1 = apap.toList
    assertTypedEquals[List[Fruit]](List(a, p, a, p), fruits1)

    val fruits2 = apbp.toList
    assertTypedEquals[List[Fruit]](List(a, p, b, p), fruits2)

    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    assertTypedEquals[APBP](apbp, fruits3.get)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.toList
    assertTypedEquals[List[Any]](List(1, "foo", 2, 3), stuff)

    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    assertTypedEquals[ISII](1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4 map isDefined
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.toList
    typed[Boolean](ll2.head)

    val moreStuff = (a :: "foo" :: p :: HNil).toList
    typed[List[Any]](moreStuff)


    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val ctv = cicscicicd.toList
    equalInferredTypes(cicscicicdList, ctv)
    assertTypedEquals[List[Ctv[Int with String with Double]]](cicscicicdList, ctv)

    val m = mimsmimimd.toList
    equalInferredTypes(mimsmimimdList, m)
    assertTypedEquals[List[M[_ >: Int with String with Double]]](mimsmimimdList, m)

    // With existentials, it gets more tricky
    val mWithEx = mimsmimemd.toList
    // Compiler fails complaining that it
    //    Cannot prove that List[HListTests.this.M[_ >: Double with _$1 with Int with String]] =:= List[HListTests.this.M[_]]
    //  equalType(mimsmimemdList, mWithEx)
    assertTypedEquals[List[M[_]]](mimsmimemdList, mWithEx)

    // Second order higher kinded types are ok...
    val m2 = m2im2sm2im2im2d.toList
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    assertTypedEquals[List[M2[_ >: Int with String with Double, Unit]]](m2im2sm2im2im2dList, m2)

    // ...as long as existentials are not involved.
    val m2e = m2eim2esm2eim2eem2ed.toList
    // Compiler complains that it
    //    Cannot prove that List[HListTests.this.M2[_ >: Double with Int with Int with String with Int, _ >: _$5 with _$3 with _$3 with _$4 with _$3]] =:= List[HListTests.this.M2[_35,_36] forSome { type _$10; type _$9; type _34 >: _$10 with _$9; type _$8; type _$7; type _32 >: _$8 with _$7; type _35 >: Double with Int with Int with String; type _36 >: _34 with _32 }]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    assertTypedEquals[List[M2[_ >: Int with String with Double, _]]](m2eim2esm2eim2eem2edList, m2e)
  }

  @Test
  def testToTraversableArray {
    def assertArrayEquals2[T](arr1 : Array[T], arr2 : Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr1.asInstanceOf[Array[Object]])

    val empty = HNil.to[Array]
    typed[Array[Nothing]](empty)
    assertArrayEquals2(Array[Nothing](), empty)
    
    implicitly[ToTraversable.Aux[HNil, Array, Nothing]]
    implicitly[ToTraversable.Aux[HNil, Array, Int]]

    val fruits1 = apap.to[Array].map(x => x : Fruit) // Default inferred type is too precise
                                                     // (Product with Serializable with Fruit)
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.to[Array].map(x => x : Fruit)
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    assertTypedEquals[APBP](apbp, fruits3.get)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.to[Array]
    typed[Array[Any]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    assertTypedEquals[ISII](1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4 map isDefined
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.to[Array]
    typed[Boolean](ll2(0))

    val moreStuff = (a :: "foo" :: p :: HNil).to[Array].map(x => x : AnyRef)
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
    //  equalType(mimsmimemdArray, mWithEx)
    typed[Array[M[_]]](mWithEx)
    assertArrayEquals2(mimsmimemdArray, mWithEx)

    val m2 = m2im2sm2im2im2d.to[Array]
    equalInferredTypes(m2im2sm2im2im2dArray, m2)
    typed[Array[M2[_ >: Int with String with Double, Unit]]](m2)
    assertArrayEquals2(m2im2sm2im2im2dArray, m2)

    val m2e = m2eim2esm2eim2eem2ed.to[Array]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    typed[Array[M2[_ >: Int with String with Double, _]]](m2e)
    assertArrayEquals2(m2im2sm2im2im2dArray.map(x => x : Any), m2e.map(x => x : Any))
  }

  @Test
  def testToArray {
    def assertArrayEquals2[T](arr1 : Array[T], arr2 : Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr1.asInstanceOf[Array[Object]])

    val empty = HNil.toArray
    typed[Array[Nothing]](empty)
    assertArrayEquals2(Array[Nothing](), empty)
    
    implicitly[ToArray[HNil, Nothing]]
    implicitly[ToArray[HNil, Int]]

    val fruits1 = apap.toArray[Fruit]
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.toArray[Fruit]
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    assertTypedEquals[APBP](apbp, fruits3.get)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.toArray
    typed[Array[Any]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    assertTypedEquals[ISII](1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4 map isDefined
    assertTypedEquals[BBBB](true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.toArray
    typed[Boolean](ll2(0))

    val moreStuff = (a :: "foo" :: p :: HNil).toArray[AnyRef]
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
    //  equalType(mimsmimemdArray, mWithEx)
    typed[Array[M[_]]](mWithEx)
    assertArrayEquals2(mimsmimemdArray, mWithEx)

    val m2 = m2im2sm2im2im2d.toArray
    equalInferredTypes(m2im2sm2im2im2dArray, m2)
    typed[Array[M2[_ >: Int with String with Double, Unit]]](m2)
    assertArrayEquals2(m2im2sm2im2im2dArray, m2)

    val m2e = m2eim2esm2eim2eem2ed.toArray
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    typed[Array[M2[_ >: Int with String with Double, _]]](m2e)
    assertArrayEquals2(m2im2sm2im2im2dArray.map(x => x : Any), m2e.map(x => x : Any))
  }

  @Test
  def testFoldMap {
    implicitly[Mapper.Aux[isDefined.type, HNil, HNil]]
    implicitly[Mapper.Aux[isDefined.type, Option[Int] :: HNil, Boolean :: HNil]]

    val tl1 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val tl2 = Option(1) :: Option("foo") :: (None : Option[Int]) :: Option(3) :: HNil

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
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val at0 = sn1(_0)
    assertTypedEquals[Int](23, at0)

    val at1 = sn1(_1)
    typed[Double](at1)
    assertEquals(3.0, at1, Double.MinPositiveValue)

    val at2 = sn1(_2)
    assertTypedEquals[String]("foo", at2)

    val at3 = sn1(_3)
    assertTypedEquals[Unit]((), at3)

    val at4 = sn1(_4)
    assertTypedEquals[String]("bar", at4)

    val at5 = sn1(_5)
    assertTypedEquals[Boolean](true, at5)

    val at6 = sn1(_6)
    assertTypedEquals[Long](5L, at6)

    val sn2 =
      0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 ::
      10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 ::
      20 :: 21 :: 22 :: HNil

    val at22 = sn2(_22)
    assertTypedEquals[Int](22, at22)
  }

  @Test
  def testAtLiteral {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val at0 = sn1(0)
    assertTypedEquals[Int](23, at0)

    val at1 = sn1(1)
    typed[Double](at1)
    assertEquals(3.0, at1, Double.MinPositiveValue)

    val at2 = sn1(2)
    assertTypedEquals[String]("foo", at2)

    val at3 = sn1(3)
    assertTypedEquals[Unit]((), at3)

    val at4 = sn1(4)
    assertTypedEquals[String]("bar", at4)

    val at5 = sn1(5)
    assertTypedEquals[Boolean](true, at5)

    val at6 = sn1(6)
    assertTypedEquals[Long](5L, at6)

    val sn2 =
      0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 ::
      10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 ::
      20 :: 21 :: 22 :: HNil

    val at22 = sn2(22)
    assertTypedEquals[Int](22, at22)
  }

  @Test
  def testTakeDrop {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val r1 = sn1.take(_0)
    assertTypedEquals[HNil](HNil, r1)

    val r2 = sn1.drop(_0)
    assertTypedEquals[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](
      23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, r2)

    val r3 = sn1.take(_2)
    assertTypedEquals[Int :: Double :: HNil](23 :: 3.0 :: HNil, r3)

    val r4 = sn1.drop(_2)
    assertTypedEquals[String :: Unit :: String :: Boolean :: Long :: HNil](
      "foo" :: () :: "bar" :: true :: 5L :: HNil, r4)

    val r5 = sn1.take(_7)
    assertTypedEquals[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](
      23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, r5)

    val r6 = sn1.drop(_7)
    assertTypedEquals[HNil](HNil, r6)
  }

  @Test
  def testTakeDropLiteral {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val r1 = sn1.take(0)
    assertTypedEquals[HNil](HNil, r1)

    val r2 = sn1.drop(0)
    assertTypedEquals[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](
      23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, r2)

    val r3 = sn1.take(2)
    assertTypedEquals[Int :: Double :: HNil](23 :: 3.0 :: HNil, r3)

    val r4 = sn1.drop(2)
    assertTypedEquals[String :: Unit :: String :: Boolean :: Long :: HNil](
      "foo" :: () :: "bar" :: true :: 5L :: HNil, r4)

    val r5 = sn1.take(7)
    assertTypedEquals[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](
      23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, r5)

    val r6 = sn1.drop(7)
    assertTypedEquals[HNil](HNil, r6)
  }

  @Test
  def testSplit {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.split(_0)
    typed[(HNil, (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](sni0)
    val sni1 = sn1.split(_1)
    typed[((Int :: HNil), (Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](sni1)
    val sni2 = sn1.split(_2)
    typed[((Int :: Double :: HNil), (String :: Unit :: String :: Boolean :: Long :: HNil))](sni2)
    val sni3 = sn1.split(_3)
    typed[((Int :: Double :: String :: HNil), (Unit :: String :: Boolean :: Long :: HNil))](sni3)
    val sni4 = sn1.split(_4)
    typed[((Int :: Double :: String :: Unit :: HNil), (String :: Boolean :: Long :: HNil))](sni4)
    val sni5 = sn1.split(_5)
    typed[((Int :: Double :: String :: Unit :: String :: HNil), (Boolean :: Long :: HNil))](sni5)
    val sni6 = sn1.split(_6)
    typed[((Int :: Double :: String :: Unit :: String :: Boolean :: HNil), (Long :: HNil))](sni6)
    val sni7 = sn1.split(_7)
    typed[((Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil), HNil)](sni7)

    val snri0 = sn1.reverse_split(_0)
    typed[(HNil, (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](snri0)
    val snri1 = sn1.reverse_split(_1)
    typed[((Int :: HNil), (Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](snri1)
    val snri2 = sn1.reverse_split(_2)
    typed[((Double :: Int :: HNil), (String :: Unit :: String :: Boolean :: Long :: HNil))](snri2)
    val snri3 = sn1.reverse_split(_3)
    typed[((String :: Double :: Int :: HNil), (Unit :: String :: Boolean :: Long :: HNil))](snri3)
    val snri4 = sn1.reverse_split(_4)
    typed[((Unit :: String :: Double :: Int :: HNil), (String :: Boolean :: Long :: HNil))](snri4)
    val snri5 = sn1.reverse_split(_5)
    typed[((String :: Unit :: String :: Double :: Int :: HNil), (Boolean :: Long :: HNil))](snri5)
    val snri6 = sn1.reverse_split(_6)
    typed[((Boolean :: String :: Unit :: String :: Double :: Int :: HNil), (Long :: HNil))](snri6)
    val snri7 = sn1.reverse_split(_7)
    typed[((Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil), HNil)](snri7)
  }

  @Test
  def testSplitLiteral {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.split(0)
    typed[(HNil, (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](sni0)
    val sni1 = sn1.split(1)
    typed[((Int :: HNil), (Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](sni1)
    val sni2 = sn1.split(2)
    typed[((Int :: Double :: HNil), (String :: Unit :: String :: Boolean :: Long :: HNil))](sni2)
    val sni3 = sn1.split(3)
    typed[((Int :: Double :: String :: HNil), (Unit :: String :: Boolean :: Long :: HNil))](sni3)
    val sni4 = sn1.split(4)
    typed[((Int :: Double :: String :: Unit :: HNil), (String :: Boolean :: Long :: HNil))](sni4)
    val sni5 = sn1.split(5)
    typed[((Int :: Double :: String :: Unit :: String :: HNil), (Boolean :: Long :: HNil))](sni5)
    val sni6 = sn1.split(6)
    typed[((Int :: Double :: String :: Unit :: String :: Boolean :: HNil), (Long :: HNil))](sni6)
    val sni7 = sn1.split(7)
    typed[((Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil), HNil)](sni7)

    val snri0 = sn1.reverse_split(0)
    typed[(HNil, (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[((Int :: HNil), (Double :: String :: Unit :: String :: Boolean :: Long :: HNil))](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[((Double :: Int :: HNil), (String :: Unit :: String :: Boolean :: Long :: HNil))](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[((String :: Double :: Int :: HNil), (Unit :: String :: Boolean :: Long :: HNil))](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[((Unit :: String :: Double :: Int :: HNil), (String :: Boolean :: Long :: HNil))](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[((String :: Unit :: String :: Double :: Int :: HNil), (Boolean :: Long :: HNil))](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[((Boolean :: String :: Unit :: String :: Double :: Int :: HNil), (Long :: HNil))](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[((Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil), HNil)](snri7)
  }

  @Test
  def testSplitP {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.splitP(_0)
    typed[(HNil) :: (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](sni0)
    val sni1 = sn1.splitP(_1)
    typed[(Int :: HNil) :: (Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](sni1)
    val sni2 = sn1.splitP(_2)
    typed[(Int :: Double :: HNil) :: (String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](sni2)
    val sni3 = sn1.splitP(_3)
    typed[(Int :: Double :: String :: HNil) :: (Unit :: String :: Boolean :: Long :: HNil) :: HNil](sni3)
    val sni4 = sn1.splitP(_4)
    typed[(Int :: Double :: String :: Unit :: HNil) :: (String :: Boolean :: Long :: HNil) :: HNil](sni4)
    val sni5 = sn1.splitP(_5)
    typed[(Int :: Double :: String :: Unit :: String :: HNil) :: (Boolean :: Long :: HNil) :: HNil](sni5)
    val sni6 = sn1.splitP(_6)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: HNil) :: (Long :: HNil) :: HNil](sni6)
    val sni7 = sn1.splitP(_7)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: (HNil) :: HNil](sni7)

    val snri0 = sn1.reverse_splitP(_0)
    typed[(HNil) :: (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](snri0)
    val snri1 = sn1.reverse_splitP(_1)
    typed[(Int :: HNil) :: (Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](snri1)
    val snri2 = sn1.reverse_splitP(_2)
    typed[(Double :: Int :: HNil) :: (String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](snri2)
    val snri3 = sn1.reverse_splitP(_3)
    typed[(String :: Double :: Int :: HNil) :: (Unit :: String :: Boolean :: Long :: HNil) :: HNil](snri3)
    val snri4 = sn1.reverse_splitP(_4)
    typed[(Unit :: String :: Double :: Int :: HNil) :: (String :: Boolean :: Long :: HNil) :: HNil](snri4)
    val snri5 = sn1.reverse_splitP(_5)
    typed[(String :: Unit :: String :: Double :: Int :: HNil) :: (Boolean :: Long :: HNil) :: HNil](snri5)
    val snri6 = sn1.reverse_splitP(_6)
    typed[(Boolean :: String :: Unit :: String :: Double :: Int :: HNil) :: (Long :: HNil) :: HNil](snri6)
    val snri7 = sn1.reverse_splitP(_7)
    typed[(Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil) :: (HNil) :: HNil](snri7)
  }

  @Test
  def testSplitPLiteral {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.splitP(0)
    typed[(HNil) :: (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](sni0)
    val sni1 = sn1.splitP(1)
    typed[(Int :: HNil) :: (Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](sni1)
    val sni2 = sn1.splitP(2)
    typed[(Int :: Double :: HNil) :: (String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](sni2)
    val sni3 = sn1.splitP(3)
    typed[(Int :: Double :: String :: HNil) :: (Unit :: String :: Boolean :: Long :: HNil) :: HNil](sni3)
    val sni4 = sn1.splitP(4)
    typed[(Int :: Double :: String :: Unit :: HNil) :: (String :: Boolean :: Long :: HNil) :: HNil](sni4)
    val sni5 = sn1.splitP(5)
    typed[(Int :: Double :: String :: Unit :: String :: HNil) :: (Boolean :: Long :: HNil) :: HNil](sni5)
    val sni6 = sn1.splitP(6)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: HNil) :: (Long :: HNil) :: HNil](sni6)
    val sni7 = sn1.splitP(7)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: (HNil) :: HNil](sni7)

    val snri0 = sn1.reverse_splitP(0)
    typed[(HNil) :: (Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](snri0)
    val snri1 = sn1.reverse_splitP(1)
    typed[(Int :: HNil) :: (Double :: String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](snri1)
    val snri2 = sn1.reverse_splitP(2)
    typed[(Double :: Int :: HNil) :: (String :: Unit :: String :: Boolean :: Long :: HNil) :: HNil](snri2)
    val snri3 = sn1.reverse_splitP(3)
    typed[(String :: Double :: Int :: HNil) :: (Unit :: String :: Boolean :: Long :: HNil) :: HNil](snri3)
    val snri4 = sn1.reverse_splitP(4)
    typed[(Unit :: String :: Double :: Int :: HNil) :: (String :: Boolean :: Long :: HNil) :: HNil](snri4)
    val snri5 = sn1.reverse_splitP(5)
    typed[(String :: Unit :: String :: Double :: Int :: HNil) :: (Boolean :: Long :: HNil) :: HNil](snri5)
    val snri6 = sn1.reverse_splitP(6)
    typed[(Boolean :: String :: Unit :: String :: Double :: Int :: HNil) :: (Long :: HNil) :: HNil](snri6)
    val snri7 = sn1.reverse_splitP(7)
    typed[(Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil) :: (HNil) :: HNil](snri7)
  }

  @Test
  def testSelect {
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
    val si = sl.select[Int]
    assertTypedEquals[Int](1, si)

    val sb = sl.select[Boolean]
    assertTypedEquals[Boolean](true, sb)

    val ss = sl.select[String]
    assertTypedEquals[String]("foo", ss)

    val sd = sl.select[Double]
    assertEquals(2.0, sd, Double.MinPositiveValue)
  }

  @Test
  def testFilter {
    val l1 = 1 :: 2 :: HNil
    val f1 = l1.filter[Int]
    assertTypedEquals[Int :: Int :: HNil](1 :: 2 :: HNil, f1)

    val l2 = 1 :: true :: "foo" :: 2 :: HNil
    val f2 = l2.filter[Int]
    assertTypedEquals[Int :: Int :: HNil](1 :: 2 :: HNil, f2)

    typed[HNil](l2.filter[Double])
  }

  @Test
  def testFilterNot {
    val l1 = 1 :: 2 :: HNil
    val f1 = l1.filterNot[String]
    assertTypedEquals[Int :: Int :: HNil](1 :: 2 :: HNil, f1)

    val l2 = 1 :: true :: "foo" :: 2 :: HNil
    val f2 = l2.filterNot[String]
    assertTypedEquals[Int :: Boolean :: Int :: HNil](1 :: true :: 2 :: HNil, f2)

    typed[HNil](l2.filter[Double])
  }

  @Test
  def testReplace {
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil

    val (i, r1) = sl.replace(23)
    assertTypedEquals[Int](1, i)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](23 :: true :: "foo" :: 2.0 :: HNil, r1)

    val (b, r2) = sl.replace(false)
    assertTypedEquals[Boolean](true, b)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: false :: "foo" :: 2.0 :: HNil, r2)

    val (s, r3) = sl.replace("bar")
    assertTypedEquals[String]("foo", s)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "bar" :: 2.0 :: HNil, r3)

    val (d, r4) = sl.replace(3.0)
    typed[Double](d)
    assertEquals(2.0, d, Double.MinPositiveValue)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "foo" :: 3.0 :: HNil, r4)

    val (i2, r5) = sl.replaceType[Int]('*')
    typed[Char](r5(0))
    assertTypedEquals[Int](1, i2)
    assertTypedEquals[Char :: Boolean :: String :: Double :: HNil]('*' :: true :: "foo" :: 2.0 :: HNil, r5)

    val (b2, r6) = sl.replaceType[Boolean]('*')
    typed[Char](r6(1))
    assertTypedEquals[Boolean](true, b2)
    assertTypedEquals[Int :: Char :: String :: Double :: HNil](1 :: '*' :: "foo" :: 2.0 :: HNil, r6)

    val (s2, r7) = sl.replaceType[String]('*')
    typed[Char](r7(2))
    assertTypedEquals[String]("foo", s2)
    assertTypedEquals[Int :: Boolean :: Char :: Double :: HNil](1 :: true :: '*' :: 2.0 :: HNil, r7)

    val (d2, r8) = sl.replaceType[Double]('*')
    typed[Double](d2)
    typed[Char](r8(3))
    assertEquals(2.0, d2, Double.MinPositiveValue)
    assertTypedEquals[Int :: Boolean :: String :: Char :: HNil](1 :: true :: "foo" :: '*' :: HNil, r8)

    val fruits = a :: p :: a :: f :: HNil
    val (x1, rr1) = fruits.replaceType[Pear](a)
    typed[Pear](x1)
    typed[Apple :: Apple :: Apple :: Fruit :: HNil](rr1)

    val (x2, rr2) = fruits.replaceType[Pear](f)
    typed[Pear](x2)
    typed[Apple :: Fruit :: Apple :: Fruit :: HNil](rr2)

    val (x3, rr3) = fruits.replaceType[Fruit](p)
    typed[Fruit](x3)
    typed[Apple :: Pear :: Apple :: Pear :: HNil](rr3)

    val (x4, rr4) = fruits.replace(p)
    typed[Pear](x4)
    typed[Apple :: Pear :: Apple :: Fruit :: HNil](rr4)

    val (x5, rr5) = fruits.replace(f)
    typed[Fruit](x5)
    typed[Apple :: Pear :: Apple :: Fruit :: HNil](rr5)
  }

  @Test
  def testUpdate {
    type SL = Int :: Boolean :: String :: Double :: HNil
    val sl: SL = 1 :: true :: "foo" :: 2.0 :: HNil

    val r1 = sl.updatedElem(23)
    assertTypedEquals[SL](23 :: true :: "foo" :: 2.0 :: HNil, r1)

    val r2 = sl.updatedElem(false)
    assertTypedEquals[SL](1 :: false :: "foo" :: 2.0 :: HNil, r2)

    val r3 = sl.updatedElem("bar")
    assertTypedEquals[SL](1 :: true :: "bar" :: 2.0 :: HNil, r3)

    val r4 = sl.updatedElem(3.0)
    assertTypedEquals[SL](1 :: true :: "foo" :: 3.0 :: HNil, r4)

    val r5 = sl.updatedType[Int]('*')
    assertTypedEquals[Char :: Boolean :: String :: Double :: HNil]('*' :: true :: "foo" :: 2.0 :: HNil, r5)

    val r6 = sl.updatedType[Boolean]('*')
    assertTypedEquals[Int :: Char :: String :: Double :: HNil](1 :: '*' :: "foo" :: 2.0 :: HNil, r6)

    val r7 = sl.updatedType[String]('*')
    assertTypedEquals[Int :: Boolean :: Char :: Double :: HNil](1 :: true :: '*' :: 2.0 :: HNil, r7)

    val r8 = sl.updatedType[Double]('*')
    assertTypedEquals(1 :: true :: "foo" :: '*' :: HNil, r8)

    val r9 = sl.updateWith((i : Int) => i * 2)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](2 :: true :: "foo" :: 2.0 :: HNil, r9)

    val r10 = sl.updateWith((b : Boolean) => !b)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: false :: "foo" :: 2.0 :: HNil, r10)

    val r11 = sl.updateWith((s : String) => s.toUpperCase)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "FOO" :: 2.0 :: HNil, r11)

    val r12 = sl.updateWith((d : Double) => d / 2.0)
    assertTypedEquals[Int :: Boolean :: String :: Double :: HNil](1 :: true :: "foo" :: 1.0 :: HNil, r12)

    val r13 = sl.updateWith((i : Int) => i.toString)
    assertTypedEquals[String :: Boolean :: String :: Double :: HNil]("1" :: true :: "foo" :: 2.0 :: HNil, r13)

    val r14 = sl.updateWith((b : Boolean) => b.toString)
    assertTypedEquals[Int :: String :: String :: Double :: HNil](1 :: "true" :: "foo" :: 2.0 :: HNil, r14)

    val r15 = sl.updateWith((_ : String) => 0xF00)
    assertTypedEquals[Int :: Boolean :: Int :: Double :: HNil](1 :: true :: 0xF00 :: 2.0 :: HNil, r15)

    val r16 = sl.updateWith((d : Double) => d.toString)
    assertTypedEquals[Int :: Boolean :: String :: String :: HNil](1 :: true :: "foo" :: "2.0" :: HNil, r16)

    val fruits = a :: p :: a :: f :: HNil

    val rr1 = fruits.updatedType[Pear](a)
    typed[Apple :: Apple :: Apple :: Fruit :: HNil](rr1)

    val rr2 = fruits.updatedType[Pear](f)
    typed[Apple :: Fruit :: Apple :: Fruit :: HNil](rr2)

    val rr3 = fruits.updatedType[Fruit](p)
    typed[Apple :: Pear :: Apple :: Pear :: HNil](rr3)

    val rr4 = fruits.updatedElem(p)
    typed[Apple :: Pear :: Apple :: Fruit :: HNil](rr4)

    val rr5 = fruits.updatedElem(f)
    typed[Apple :: Pear :: Apple :: Fruit :: HNil](rr5)
  }

  @Test
  def testSplitLeft {
    type SL  = Int :: Boolean :: String :: Double :: HNil
    type SL2 = Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil
    val sl: SL   = 1 :: true :: "foo" :: 2.0 :: HNil
    val sl2: SL2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val (sp1, sp2) = sl.splitLeft[String]
    typed[String :: Double :: HNil](sp2)
    typed[Int :: Boolean :: HNil](sp1)
    assertTypedEquals[SL]((sp1 ::: sp2), sl)

    val (sli1, sli2) = sl2.splitLeft[String]
    typed[Int :: Double :: HNil](sli1)
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](sli2)
    assertTypedEquals[SL2]((sli1 ::: sli2), sl2)

    val (rsp1, rsp2) = sl.reverse_splitLeft[String]
    typed[Boolean :: Int :: HNil](rsp1)
    typed[String :: Double :: HNil](rsp2)
    assertTypedEquals[SL]((rsp1 reverse_::: rsp2), sl)

    val (rsli1, rsli2) = sl2.reverse_splitLeft[String]
    typed[Double :: Int :: HNil](rsli1)
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](rsli2)
    assertTypedEquals[SL2]((rsli1 reverse_::: rsli2), sl2)
  }

  @Test
  def testSplitLeftP {
    type SL  = Int :: Boolean :: String :: Double :: HNil
    type SL2 = Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil
    val sl: SL   = 1 :: true :: "foo" :: 2.0 :: HNil
    val sl2: SL2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sp1 :: sp2 :: HNil = sl.splitLeftP[String]
    typed[String :: Double :: HNil](sp2)
    typed[Int :: Boolean :: HNil](sp1)
    assertTypedEquals[SL]((sp1 ::: sp2), sl)

    val sli1 :: sli2 :: HNil = sl2.splitLeftP[String]
    typed[Int :: Double :: HNil](sli1)
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](sli2)
    assertTypedEquals[SL2]((sli1 ::: sli2), sl2)

    val rsp1 :: rsp2 :: HNil = sl.reverse_splitLeftP[String]
    typed[Boolean :: Int :: HNil](rsp1)
    typed[String :: Double :: HNil](rsp2)
    assertTypedEquals[SL]((rsp1 reverse_::: rsp2), sl)

    val rsli1 :: rsli2 :: HNil = sl2.reverse_splitLeftP[String]
    typed[Double :: Int :: HNil](rsli1)
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](rsli2)
    assertTypedEquals[SL2]((rsli1 reverse_::: rsli2), sl2)
  }

  @Test
  def testSplitRight {
    type SL  = Int :: Boolean :: String :: Double :: HNil
    type SL2 = Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil
    val sl: SL   = 1 :: true :: "foo" :: 2.0 :: HNil
    val sl2: SL2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val (srp1, srp2) = sl.splitRight[String]
    typed[Int :: Boolean :: String :: HNil](srp1)
    typed[Double :: HNil](srp2)
    assertTypedEquals[SL]((srp1 ::: srp2), sl)

    val (srli1, srli2) = sl2.splitRight[String]
    typed[Int :: Double :: String :: Unit :: String :: HNil](srli1)
    typed[Boolean :: Long :: HNil](srli2)
    assertTypedEquals[SL2](sl2, srli1 ::: srli2)

    val (rsrp1, rsrp2) = sl.reverse_splitRight[String]
    typed[String :: Boolean :: Int :: HNil](rsrp1)
    typed[Double :: HNil](rsrp2)
    assertTypedEquals[SL]((rsrp1 reverse_::: rsrp2), sl)

    val (rsrli1, rsrli2) = sl2.reverse_splitRight[String]
    typed[String :: Unit :: String :: Double :: Int :: HNil](rsrli1)
    typed[Boolean :: Long :: HNil](rsrli2)
    assertTypedEquals[SL2]((rsrli1 reverse_::: rsrli2), sl2)
  }

  @Test
  def testSplitRightP {
    type SL  = Int :: Boolean :: String :: Double :: HNil
    type SL2 = Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil
    val sl: SL   = 1 :: true :: "foo" :: 2.0 :: HNil
    val sl2: SL2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val srp1 :: srp2 :: HNil = sl.splitRightP[String]
    typed[Int :: Boolean :: String :: HNil](srp1)
    typed[Double :: HNil](srp2)
    assertTypedEquals[SL]((srp1 ::: srp2), sl)

    val srli1 :: srli2 :: HNil = sl2.splitRightP[String]
    typed[Int :: Double :: String :: Unit :: String :: HNil](srli1)
    typed[Boolean :: Long :: HNil](srli2)
    assertTypedEquals[SL2](sl2, srli1 ::: srli2)

    val rsrp1 :: rsrp2 :: HNil = sl.reverse_splitRightP[String]
    typed[String :: Boolean :: Int :: HNil](rsrp1)
    typed[Double :: HNil](rsrp2)
    assertTypedEquals[SL]((rsrp1 reverse_::: rsrp2), sl)

    val rsrli1 :: rsrli2 :: HNil = sl2.reverse_splitRightP[String]
    typed[String :: Unit :: String :: Double :: Int :: HNil](rsrli1)
    typed[Boolean :: Long :: HNil](rsrli2)
    assertTypedEquals[SL2]((rsrli1 reverse_::: rsrli2), sl2)
  }

  @Test
  def testTranspose {
    val l1 = 1 :: HNil
    val l2 = ("a" :: HNil) :: HNil

    val r1 = l1.zipOne(l2)
    assertTypedEquals[(Int :: String :: HNil) :: HNil]((1 :: "a" :: HNil) :: HNil, r1)
    val r2 = l1.mapConst(HNil)
    assertTypedEquals[HNil :: HNil](HNil :: HNil, r2)
    val r3 = (l1 :: HNil).transpose
    assertTypedEquals[(Int :: HNil) :: HNil]((1 :: HNil) :: HNil, r3)

    val l3 = 1 :: 2 :: 3 :: HNil
    val l4 = ("a" :: 1.0 :: HNil) :: ("b" :: 2.0 :: HNil) :: ("c" :: 3.0 :: HNil) :: HNil

    type ISD = Int :: String :: Double :: HNil
    val z2 = l3.zipOne(l4)
    assertTypedEquals[ISD :: ISD :: ISD :: HNil](
      (1 :: "a" :: 1.0 :: HNil) :: (2 :: "b" :: 2.0 :: HNil) :: (3 :: "c" :: 3.0 :: HNil) :: HNil, z2
    )

    val r5 = l3.mapConst(HNil)
    assertTypedEquals[HNil :: HNil :: HNil :: HNil](HNil :: HNil :: HNil :: HNil, r5)

    val t2 = l4.transpose
    assertTypedEquals[
      (String :: String :: String :: HNil) ::
      (Double :: Double :: Double :: HNil) :: HNil
    ](("a" :: "b" :: "c" :: HNil) :: (1.0 :: 2.0 :: 3.0 :: HNil) :: HNil, t2)

    val t3 = z2.transpose
    assertTypedEquals[
      (Int :: Int :: Int :: HNil) ::
      (String :: String :: String :: HNil) ::
      (Double :: Double :: Double :: HNil) :: HNil
    ](
      (1 :: 2 :: 3 :: HNil) ::
      ("a" :: "b" :: "c" :: HNil) ::
      (1.0 :: 2.0 :: 3.0 :: HNil) :: HNil,
      t3
    )

    val r8 = t3.transpose
    assertTypedEquals[ISD :: ISD :: ISD :: HNil](z2, r8)
  }

  @Test
  def testZipUnzip {
    val l1 = 1 :: "a" :: 1.0 :: HNil
    val l2 = 2 :: "b" :: 2.0 :: HNil

    val t1 = (l1 :: l2 :: HNil).transpose
    val z1 = t1.map(tupled)
    assertTypedEquals[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](
      (1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z1)

    def zip[L <: HList, OutT <: HList](l : L)
      (implicit
        transposer : Transposer.Aux[L, OutT],
        mapper : Mapper[tupled.type, OutT]) = l.transpose.map(tupled)

    val z2 = zip(l1 :: l2 :: HNil)
    assertTypedEquals[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](
      (1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z2)

    val z3 = (l1 :: l2 :: HNil).zip
    assertTypedEquals[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](
      (1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z3)

    val t2 = z1.map(productElements).transpose
    val u1 = t2.tupled
    assertTypedEquals[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](
      (1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), u1)

    def unzip[L <: HList, OutM <: HList, OutT <: HList](l : L)
      (implicit
        mapper : Mapper.Aux[productElements.type, L, OutM],
        transposer : Transposer.Aux[OutM, OutT],
        tupler : Tupler[OutT]) = l.map(productElements).transpose.tupled

    val u2 = unzip(z1)
    assertTypedEquals[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](
      (1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), u2)

    val r1 = z1.unzip
    assertTypedEquals[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](
      (1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), r1)

    val r2 = l1 zip l2
    assertTypedEquals[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](
      (1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, r2)

    val intInc : Int => Int = _+1
    val stringInc : String => String = _+"*"
    val doubleInc : Double => Int = _.toInt+1

    val l3 = intInc :: stringInc :: doubleInc :: HNil

    val z5 = l3 zipApply l1
    assertTypedEquals[Int :: String :: Int :: HNil](2 :: "a*" :: 2 :: HNil, z5)
  }

  @Test
  def testUnapply {
    val l = 1 :: true :: "foo" :: 2.0 :: HNil
    val l2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val is = l match {
      case i :: true :: s :: 2.0 :: HNil => (i, s)
    }

    assertTypedEquals[Int](1, is._1)
    assertTypedEquals[String]("foo", is._2)

    val is2 = (l : Any) match {
      case (i : Int) :: true :: (s : String) :: 2.0 :: HNil => (i, s)
      case _ => sys.error("Not matched")
    }

    assertTypedEquals[Int](1, is2._1)
    assertTypedEquals[String]("foo", is2._2)

    import HList.ListCompat._

    val tl = l2 match {
      case 23 #: 3.0 #: s #: xs => (s, xs)
    }

    assertTypedEquals[String]("foo", tl._1)
    assertTypedEquals[Unit :: String :: Boolean :: Long :: HNil](() :: "bar" :: true :: 5L :: HNil, tl._2)

    val tl2 = (l2 : Any) match {
      case 23 #: 3.0 #: (s : String) #: xs => (s, xs)
      case _ => sys.error("Not matched")
    }

    assertTypedEquals[String]("foo", tl2._1)
    assertTypedEquals[HList](() :: "bar" :: true :: 5L :: HNil, tl2._2)

    val ll = List(1, 2, 3, 4)
    val tll = ll match {
      case 1 :: 2 :: x :: y :: Nil => (x, y)
      case _ => sys.error("Not matched")
    }
    assertTypedEquals[Int](3, tll._1)
    assertTypedEquals[Int](4, tll._2)

    val tll2 = ll match {
      case 1 :: xs => xs
      case _ => sys.error("Not matched")
    }
    assertTypedEquals[List[Int]](List(2, 3, 4), tll2)

    val mixed = 23 :: "foo" :: (1 :: 2 :: 3 :: 4 :: 5 :: Nil) :: false :: () :: HNil
    val tmixed = mixed match {
      case _ #: _ #: (_ :: 2 :: x :: tl1) #: tl2 => (x, tl1, tl2)
      case _ => sys.error("Not matched")
    }
    assertTypedEquals[Int](3, tmixed._1)
    assertTypedEquals[List[Int]](4 :: 5 :: Nil, tmixed._2)
    assertTypedEquals[Boolean :: Unit :: HNil](false :: () :: HNil, tmixed._3)
  }

  @Test
  def testRemove {
    val l = 1 :: true :: "foo" :: HNil

    val li = l.removeElem[Int]
    assertTypedEquals[(Int, Boolean :: String :: HNil)]((1, true :: "foo" :: HNil), li)

    val lb = l.removeElem[Boolean]
    assertTypedEquals[(Boolean, Int :: String :: HNil)]((true, 1 :: "foo" :: HNil), lb)

    val ls = l.removeElem[String]
    assertTypedEquals[(String, Int :: Boolean :: HNil)](("foo", 1 :: true :: HNil), ls)
  }

  @Test
  def testRemoveAll {
    val l = 1 :: true :: "foo" :: HNil

    val lnil = l.removeAll[HNil]
    assertTypedEquals[(HNil, Int :: Boolean :: String :: HNil)]((HNil, 1 :: true :: "foo" :: HNil), lnil)

    val li = l.removeAll[Int :: HNil]
    assertTypedEquals[(Int :: HNil, Boolean :: String :: HNil)]((1 :: HNil, true :: "foo" :: HNil), li)

    val lb = l.removeAll[Boolean :: HNil]
    assertTypedEquals[(Boolean :: HNil, Int :: String :: HNil)]((true :: HNil, 1 :: "foo" :: HNil), lb)

    val lbi = l.removeAll[Boolean :: Int :: HNil]
    assertTypedEquals[(Boolean :: Int :: HNil, String :: HNil)]((true :: 1 :: HNil, "foo" :: HNil), lbi)
  }

  object combine extends Poly {
    implicit def caseCharString = use((c : Char, s : String) => s.indexOf(c))
    implicit def caseIntBoolean = use((i : Int, b : Boolean) => if ((i >= 0) == b) "pass" else "fail")
  }

  @Test
  def testFoldLeft {
    val c1a = combine('o', "foo")
    val c1b = combine(c1a, true)
    assertTypedEquals[String]("pass", c1b)

    implicitly[LeftFolder.Aux[HNil, String, combine.type, String]]
    implicitly[LeftFolder.Aux[Boolean :: HNil, Int, combine.type, String]]
    implicitly[LeftFolder.Aux[String :: Boolean :: HNil, Char, combine.type, String]]

    val tf1 = implicitly[LeftFolder[HNil, String, combine.type]]
    val tf2 = implicitly[LeftFolder[Boolean :: HNil, Int, combine.type]]
    val tf3 = implicitly[LeftFolder[String :: Boolean :: HNil, Char, combine.type]]

    val l1 = "foo" :: true :: HNil
    val f1 = l1.foldLeft('o')(combine)
    assertTypedEquals[String]("pass", f1)

    val c2a = combine('o', "bar")
    val c2b = combine(c2a, false)
    assertTypedEquals[String]("pass", c2b)

    val l2 = "bar" :: false :: HNil
    val f2 = l2.foldLeft('o')(combine)
    assertTypedEquals[String]("pass", f2)
  }

  @Test
  def testUpdatedAt {
    type IBS = Int :: Boolean :: String :: HNil
    val l = 1 :: true :: "foo" :: HNil

    val r1 = l.updatedAt[_0](2)
    assertTypedEquals[IBS](2 ::  true :: "foo" :: HNil, r1)

    val r2 = l.updatedAt[_1](false)
    assertTypedEquals[IBS](1 :: false :: "foo" :: HNil, r2)

    val r3 = l.updatedAt[_2]("bar")
    assertTypedEquals[IBS](1 ::  true :: "bar" :: HNil, r3)
  }

  @Test
  def testUpdatedAtLiteral {
    type IBS = Int :: Boolean :: String :: HNil
    val l = 1 :: true :: "foo" :: HNil

    val r1 = l.updatedAt(0, 2)
    assertTypedEquals[IBS](2 ::  true :: "foo" :: HNil, r1)

    val r2 = l.updatedAt(1, false)
    assertTypedEquals[IBS](1 :: false :: "foo" :: HNil, r2)

    val r3 = l.updatedAt(2, "bar")
    assertTypedEquals[IBS](1 ::  true :: "bar" :: HNil, r3)
  }

  @Test
  def testNatTRel {
    type L1 = Int :: String :: Boolean :: HNil
    type L2 = List[Int] :: List[String] :: List[Boolean] :: HNil
    type L3 = Option[Int] :: Option[String] :: Option[Boolean] :: HNil
    type L4 = Int :: Int :: Int :: HNil
    type L5 = String :: String :: String :: HNil

    implicitly[NatTRel[L1, Id, L2, List]]
    implicitly[NatTRel[L2, List, L1, Id]]

    implicitly[NatTRel[L2, List, L3, Option]]

    implicitly[NatTRel[L1, Id, L4, Const[Int]#λ]]

    implicitly[NatTRel[L2, List, L4, Const[Int]#λ]]
  }

  object optionToList extends (Option ~> List) {
    def apply[A](fa: Option[A]): List[A] = List.fill(3)(fa.toList).flatten
  }

  @Test
  def testNatTRelMap {
    type L1 = Option[Int] :: Option[Boolean] :: Option[String] :: Option[Nothing] :: HNil
    type L2 = List[Int] :: List[Boolean] :: List[String] :: List[Nothing] :: HNil
    val nattrel = implicitly[NatTRel[L1, Option, L2, List]]

    val l1: L1 = Option(1) :: Option(true) :: Option("three") :: None :: HNil
    val l2 = nattrel.map(optionToList, l1)

    assertTypedEquals[L2](l2,
      List(1, 1, 1) :: List(true, true, true) :: List("three", "three", "three") :: List() :: HNil)
  }

  @Test
  def testZipConst {
    type IBS = Int :: Boolean :: String :: HNil
    val c = 5
    type WithConst = (Int, Int) :: (Boolean, Int) :: (String, Int) :: HNil
    val l = 1 :: true :: "a" :: HNil
    typed[IBS](l)
    val expected = (1, c) :: (true, c) :: ("a", c) :: HNil

    val zcIntIbs = ZipConst[Int, IBS]
    val zipped1 = zcIntIbs(c, l)
    assertTypedEquals[WithConst](expected, zipped1)

    val zcaIntIbs = implicitly[ZipConst.Aux[Int, IBS, WithConst]]
    assertTypedEquals[WithConst](expected, zcaIntIbs(c, l))

    val x = l.zipConst(c)
    assertTypedEquals[WithConst](expected, x)
  }

  @Test
  def testZipWith {
    import poly._

    object empty extends Poly2

    object add extends Poly2 {
      implicit val caseIntInt = at[Int, Int](_ + _)
    }

    // HNil zipWith HNil (emptyFn)
    val r1 = (HNil: HNil).zipWith(HNil: HNil)(empty)
    assertTypedEquals[HNil](HNil, r1)

    // HNil zipWith nonEmpty (emptyFn)
    val r2 = (HNil: HNil).zipWith(1 :: HNil)(empty)
    assertTypedEquals[HNil](HNil, r2)

    // nonEmpty zipWith HNil (emptyFn)
    val r3 = (1 :: HNil).zipWith(HNil: HNil)(empty)
    assertTypedEquals[HNil](HNil, r3)

    // singleton zipWith singleton
    val r4 = (1 :: HNil).zipWith(2 :: HNil)(add)
    assertTypedEquals[Int :: HNil](3 :: HNil, r4)

    { // longList zipWith longerList
      type Left  = Int :: String :: Double :: HNil
      type Right = Int :: Double :: String :: Boolean :: HNil

      val left: Left   = 1 :: "foo" :: 1.2 :: HNil
      val right: Right = 2 :: 2.3 :: "3.4" :: true :: HNil

      object zipFn extends Poly2 {
        implicit val caseIntInt       = at[Int, Int](_ + _)
        implicit val caseStringDouble = at[String, Double](_ + " -> " + _.toString)
        implicit val caseDoubleString = at[Double, String](_ + _.toDouble)
      }

      val r5 = left.zipWith(right)(zipFn)
      assertTypedEquals[Int :: String :: Double :: HNil](3 :: "foo -> 2.3" :: 4.6 :: HNil, r5)
    }

    { // invalid polys
      illTyped("""
        (1 :: HNil).zipWith(2 :: HNil)(empty)
      """)

      object noIntFn extends Poly2 {
        implicit val caseDoubleDouble = at[Double, Double](_ + _)
      }

      illTyped("""
        (1 :: HNil).zipWith(2 :: HNil)(noIntFn)
      """)

      illTyped("""
        (1.0 :: 2 :: HNil).zipWith(2.0 :: 3 :: HNil)(noIntFn)
      """)
    }
  }

  @Test
  def testWithKeys {
    import record._
    import syntax.singleton._

    val orig =
      ("intField" ->> 1) ::
      ("boolField" ->> true) ::
      HNil

    val result = orig.values.zipWithKeys(orig.keys)
    sameTyped(orig)(result)
    assertEquals(orig, result)
    val int = result.get("intField")
    assertTypedEquals[Int](1, int)
    val bool = result.get("boolField")
    assertTypedEquals[Boolean](true, bool)
    illTyped("""result.get("otherField")""")

    // key/value lengths must match up
    illTyped("orig.tail.values.zipWithKeys(orig.keys)")
    illTyped("orig.values.zipWithKeys(orig.keys.tail)")
    
    // Explicit type argument
    {
      val result = orig.values.zipWithKeys[Literals.`"intField", "boolField"`.T]
      sameTyped(orig)(result)
      assertEquals(orig, result)
      val int = result.get("intField")
      assertTypedEquals[Int](1, int)
      val bool = result.get("boolField")
      assertTypedEquals[Boolean](true, bool)
      illTyped("""result.get("otherField")""")

      // key/value lengths must match up
      illTyped(""" orig.tail.values.zipWithKeys[Literals.`"intField", "boolField"`.T] """)
      illTyped(""" orig.values.zipWithKeys[Literals.`"boolField"`.T] """)
    }
  }

  @Test
  def testCollect {
    import poly._

    object empty extends Poly1

    object complex extends Poly1 {
      implicit val caseInt    = at[Int](_.toDouble)
      implicit val caseString = at[String](_ => 1)
    }

    val in: Int :: String :: Double :: HNil = 1 :: "foo" :: 2.2 :: HNil

    // HNil collect p
    val r1 = (HNil: HNil).collect(empty)
    assertTypedEquals[HNil](HNil, r1)

    val r2 = (HNil: HNil).collect(poly.identity)
    assertTypedEquals[HNil](HNil, r2)

    val r3 = (HNil: HNil).collect(complex)
    assertTypedEquals[HNil](HNil, r3)

    // non-HNil collect empty
    val r4 = in.collect(empty)
    assertTypedEquals[HNil](HNil, r4)

    // non-HNil collect identity
    val r5 = in.collect(identity)
    assertTypedEquals[Int :: String :: Double :: HNil](in, r5)

    // non-HNil collect complex
    val r6 = in.collect(complex)
    assertTypedEquals[Double :: Int :: HNil](1.0 :: 1 :: HNil, r6)
  }

  @Test
  def testOrdering {
    assertEquals(List(HNil: HNil, HNil), List(HNil: HNil, HNil).sorted)

    assertEquals(List(1 :: HNil, 2 :: HNil, 3 :: HNil), List(2 :: HNil, 1 :: HNil, 3 :: HNil).sorted)

    assertEquals(
      List(1 :: "abc" :: HNil, 1 :: "def" :: HNil, 2 :: "abc" :: HNil, 2 :: "def" :: HNil),
      List(2 :: "abc" :: HNil, 1 :: "def" :: HNil, 2 :: "def" :: HNil, 1 :: "abc" :: HNil).sorted
    )
  }

  @Test
  def testMapCons {
    type C = Char; type S = String; type I = Int; type D = Double

    val r1 = (HNil: HNil).mapCons('a')
    assertTypedEquals[HNil](HNil, r1)

    val r2 = (HNil :: HNil).mapCons('a')
    assertTypedEquals[(Char :: HNil) :: HNil]((('a' :: HNil) :: HNil), r2)

    val r3 = ((1 :: HNil) :: ("foo" :: HNil) :: (2.0 :: HNil) :: HNil).mapCons('a')
    assertTypedEquals[(C::I::HNil) :: (C::S::HNil) :: (C::D::HNil) :: HNil](
      ('a' :: 1 :: HNil) :: ('a' :: "foo" :: HNil) :: ('a' :: 2.0 :: HNil) :: HNil,
      r3
    )
  }

  @Test
  def testInterleave {
    type C = Char; type S = String; type I = Int; type D = Double
    def interleave[I, L <: HList](i: I, l: L)(implicit interleave: Interleave[I, L]): interleave.Out = interleave(i, l)

    val r1 = interleave('i', HNil)
    assertTypedEquals[(Char :: HNil) :: HNil](('i' :: HNil) :: HNil, r1)

    val r2 = interleave('i', 1 :: HNil)
    assertTypedEquals[(C::I::HNil) :: (I::C::HNil) :: HNil](('i' :: 1 :: HNil) :: (1 :: 'i' :: HNil) :: HNil,
      r2
    )

    val r3 = interleave('i', 1 :: "foo" :: HNil)
    assertTypedEquals[(C::I::S::HNil) :: (I::C::S::HNil) :: (I::S::C::HNil) :: HNil](
      ('i' :: 1 :: "foo" :: HNil) ::
      (1 :: 'i' :: "foo" :: HNil) ::
      (1 :: "foo" :: 'i' :: HNil) :: HNil,
      r3
    )

    val r4 = interleave('i', 1 :: "foo" :: 2.0 :: HNil)
    assertTypedEquals[(C::I::S::D::HNil) :: (I::C::S::D::HNil) :: (I::S::C::D::HNil) :: (I::S::D::C::HNil) :: HNil](
      ('i' :: 1 :: "foo" :: 2.0 :: HNil) ::
      (1 :: 'i' :: "foo" :: 2.0 :: HNil) ::
      (1 :: "foo" :: 'i' :: 2.0 :: HNil) ::
      (1 :: "foo" :: 2.0 :: 'i' :: HNil) :: HNil,
      r4
    )
  }

  @Test
  def testFlatMapInterleave {
    type C = Char; type I = Int

    def flatMapInterleave[I, L <: HList](i: I, l: L)(implicit flatMapInterleave: FlatMapInterleave[I, L]) =
      flatMapInterleave(i, l)

    val r1 = flatMapInterleave('i', HNil)
    assertTypedEquals[HNil](HNil, r1)

    val r2 = flatMapInterleave('i', HNil :: HNil)
    assertTypedEquals[(Char :: HNil) :: HNil](('i' :: HNil) :: HNil, r2)

    val r3 = flatMapInterleave('i', (1 :: HNil) :: (2 :: HNil) :: HNil)
    assertTypedEquals[(C::I::HNil) :: (I::C::HNil) :: (C::I::HNil) :: (I::C::HNil) :: HNil](
      ('i' :: 1 :: HNil) ::
      (1 :: 'i' :: HNil) ::
      ('i' :: 2 :: HNil) ::
      (2 :: 'i' :: HNil) :: HNil,
      r3
    )
  }

  @Test
  def testPermutations {
    type S = String; type I = Int; type D = Double

    val r1 = HNil.permutations
    assertTypedEquals[HNil :: HNil](HNil :: HNil, r1)

    val r2 = (1 :: HNil).permutations
    assertTypedEquals[(Int :: HNil) :: HNil]((1 :: HNil) :: HNil, r2)

    val r3 = (1 :: "foo" :: HNil).permutations
    assertTypedEquals[(I::S::HNil) :: (S::I::HNil) :: HNil](
      (1 :: "foo" :: HNil) ::
      ("foo" :: 1 :: HNil) :: HNil,
      r3
    )

    val r4 = (1 :: "foo" :: 2.0 :: HNil).permutations
    assertTypedEquals[
      (I::S::D::HNil) :: (S::I::D::HNil) :: (S::D::I::HNil) ::
      (I::D::S::HNil) :: (D::I::S::HNil) :: (D::S::I::HNil) :: HNil
    ](
      (1 :: "foo" :: 2.0 :: HNil) ::
      ("foo" :: 1 :: 2.0 :: HNil) ::
      ("foo" :: 2.0 :: 1 :: HNil) ::
      (1 :: 2.0 :: "foo" :: HNil) ::
      (2.0 :: 1 :: "foo" :: HNil) ::
      (2.0 :: "foo" :: 1 :: HNil) :: HNil,
      r4
    )
  }

  @Test
  def testMkString {
    assertEquals("⸨1, foo, 2.0⸩", (1 :: "foo" :: 2.0 :: HNil).mkString("⸨", ", ", "⸩"))
  }

  @Test
  def testRotateLeft {
    val in0 = HNil
    val in1 = 1 :: HNil
    val in2 = 1 :: "foo" :: HNil
    val in3 = 1 :: "foo" :: 2.0 :: HNil
    val in4 = 1 :: "foo" :: 2.0 :: 'a' :: HNil
    type S = String; type I = Int; type D = Double; type C = Char

    { // rotateLeft(0)
      val r1 = in0.rotateLeft(0)
      assertTypedSame[HNil](HNil, r1)
      val r2 = in1.rotateLeft(0)
      assertTypedSame[I :: HNil](in1, r2)
      val r3 = in2.rotateLeft(0)
      assertTypedSame[I :: S :: HNil](in2, r3)
      val r4 = in3.rotateLeft(0)
      assertTypedSame[I :: S :: D :: HNil](in3, r4)
      val r5 = in4.rotateLeft(0)
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r5)
    }

    { // rotateLeft[_0]
      val r1 = in0.rotateLeft[_0]
      assertTypedSame[HNil](HNil, r1)
      val r2 = in1.rotateLeft[_0]
      assertTypedSame[I :: HNil](in1, r2)
      val r3 = in2.rotateLeft[_0]
      assertTypedSame[I :: S :: HNil](in2, r3)
      val r4 = in3.rotateLeft[_0]
      assertTypedSame[I :: S :: D :: HNil](in3, r4)
      val r5 = in4.rotateLeft[_0]
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r5)
    }

    { // rotateLeft(n % size == 0)
      val r1 = in1.rotateLeft(1)
      assertTypedSame[I :: HNil](in1, r1)
      val r2 = in1.rotateLeft(2)
      assertTypedSame[I :: HNil](in1, r2)
      val r3 = in2.rotateLeft(2)
      assertTypedSame[I :: S :: HNil](in2, r3)
      val r4 = in2.rotateLeft(4)
      assertTypedSame[I :: S :: HNil](in2, r4)
      val r5 = in3.rotateLeft(3)
      assertTypedSame[I :: S :: D :: HNil](in3, r5)
      val r6 = in3.rotateLeft(6)
      assertTypedSame[I :: S :: D :: HNil](in3, r6)
      val r7 = in4.rotateLeft(4)
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r7)
      val r8 = in4.rotateLeft(8)
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r8)
    }

    { // rotateLeft[N % Size == 0]
    val r1 = in1.rotateLeft[_1]
      assertTypedSame[I :: HNil](in1, r1)
      val r2 = in1.rotateLeft[_2]
      assertTypedSame[I :: HNil](in1, r2)
      val r3 = in2.rotateLeft[_2]
      assertTypedSame[I :: S :: HNil](in2, r3)
      val r4 = in2.rotateLeft[_4]
      assertTypedSame[I :: S :: HNil](in2, r4)
      val r5 = in3.rotateLeft[_3]
      assertTypedSame[I :: S :: D :: HNil](in3, r5)
      val r6 = in3.rotateLeft[_6]
      assertTypedSame[I :: S :: D :: HNil](in3, r6)
      val r7 = in4.rotateLeft[_4]
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r7)
      val r8 = in4.rotateLeft[_8]
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r8)
    }

    { // other(n)
      val r1 = in2.rotateLeft(1)
      assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, r1)

      val r2 = in3.rotateLeft(1)
      assertTypedEquals[S :: D :: I :: HNil]("foo" :: 2.0 :: 1 :: HNil, r2)

      val r3 = in4.rotateLeft(1)
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r3)

      val r4 = in4.rotateLeft(2)
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r4)

      val r5 = in4.rotateLeft(3)
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r5)

      val r6 = in4.rotateLeft(5)
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r6)

      val r7 = in4.rotateLeft(6)
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r7)
    }

    { // other[N]
    val r1 = in2.rotateLeft[_1]
      assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, r1)

      val r2 = in3.rotateLeft[_1]
      assertTypedEquals[S :: D :: I :: HNil]("foo" :: 2.0 :: 1 :: HNil, r2)

      val r3 = in4.rotateLeft[_1]
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r3)

      val r4 = in4.rotateLeft[_2]
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r4)

      val r5 = in4.rotateLeft[_3]
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r5)

      val r6 = in4.rotateLeft[_5]
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r6)

      val r7 = in4.rotateLeft[_6]
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r7)
    }
  }

  @Test
  def testRotateRight {
    val in0 = HNil
    val in1 = 1 :: HNil
    val in2 = 1 :: "foo" :: HNil
    val in3 = 1 :: "foo" :: 2.0 :: HNil
    val in4 = 1 :: "foo" :: 2.0 :: 'a' :: HNil
    type S = String; type I = Int; type D = Double; type C = Char

    { // rotateRight(0)
      val r1 = in0.rotateRight(0)
      assertTypedSame[HNil](HNil, r1)
      val r2 = in1.rotateRight(0)
      assertTypedSame[I :: HNil](in1, r2)
      val r3 = in2.rotateRight(0)
      assertTypedSame[I :: S :: HNil](in2, r3)
      val r4 = in3.rotateRight(0)
      assertTypedSame[I :: S :: D :: HNil](in3, r4)
      val r5 = in4.rotateRight(0)
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r5)
    }

    { // rotateRight[_0]
      val r1 = in0.rotateRight[_0]
      assertTypedSame[HNil](HNil, r1)
      val r2 = in1.rotateRight[_0]
      assertTypedSame[I :: HNil](in1, r2)
      val r3 = in2.rotateRight[_0]
      assertTypedSame[I :: S :: HNil](in2, r3)
      val r4 = in3.rotateRight[_0]
      assertTypedSame[I :: S :: D :: HNil](in3, r4)
      val r5 = in4.rotateRight[_0]
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r5)
    }

    { // rotateRight(n % size == 0)
      val r1 = in1.rotateRight(1)
      assertTypedSame[I :: HNil](in1, r1)
      val r2 = in1.rotateRight(2)
      assertTypedSame[I :: HNil](in1, r2)
      val r3 = in2.rotateRight(2)
      assertTypedSame[I :: S :: HNil](in2, r3)
      val r4 = in2.rotateRight(4)
      assertTypedSame[I :: S :: HNil](in2, r4)
      val r5 = in3.rotateRight(3)
      assertTypedSame[I :: S :: D :: HNil](in3, r5)
      val r6 = in3.rotateRight(6)
      assertTypedSame[I :: S :: D :: HNil](in3, r6)
      val r7 = in4.rotateRight(4)
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r7)
      val r8 = in4.rotateRight(8)
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r8)
    }

    { // rotateRight[N % Size == 0]
      val r1 = in1.rotateRight[_1]
      assertTypedSame[I :: HNil](in1, r1)
      val r2 = in1.rotateRight[_2]
      assertTypedSame[I :: HNil](in1, r2)
      val r3 = in2.rotateRight[_2]
      assertTypedSame[I :: S :: HNil](in2, r3)
      val r4 = in2.rotateRight[_4]
      assertTypedSame[I :: S :: HNil](in2, r4)
      val r5 = in3.rotateRight[_3]
      assertTypedSame[I :: S :: D :: HNil](in3, r5)
      val r6 = in3.rotateRight[_6]
      assertTypedSame[I :: S :: D :: HNil](in3, r6)
      val r7 = in4.rotateRight[_4]
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r7)
      val r8 = in4.rotateRight[_8]
      assertTypedSame[I :: S :: D :: C :: HNil](in4, r8)
    }

    { // others(n)
      val r1 = in2.rotateRight(1)
      assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, r1)

      val r2 = in3.rotateRight(1)
      assertTypedEquals[D :: I :: S :: HNil](2.0 :: 1 :: "foo" :: HNil, r2)

      val r3 = in4.rotateRight(1)
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r3)

      val r4 = in4.rotateRight(2)
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r4)

      val r5 = in4.rotateRight(3)
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r5)

      val r6 = in4.rotateRight(5)
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r6)

      val r7 = in4.rotateRight(6)
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r7)
    }

    { // others[N]
      val r1 = in2.rotateRight[_1]
      assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, r1)

      val r2 = in3.rotateRight[_1]
      assertTypedEquals[D :: I :: S :: HNil](2.0 :: 1 :: "foo" :: HNil, r2)

      val r3 = in4.rotateRight[_1]
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r3)

      val r4 = in4.rotateRight[_2]
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r4)

      val r5 = in4.rotateRight[_3]
      assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, r5)

      val r6 = in4.rotateRight[_5]
      assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, r6)

      val r7 = in4.rotateRight[_6]
      assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, r7)
    }
  }

  object smear extends Poly {
    implicit val caseIntInt    = use((x: Int, y: Int) => x + y)
    implicit val caseStringInt = use((x: String, y: Int) => x.toInt + y)
    implicit val caseIntString = use((x: Int, y: String) => x + y.toInt)
  }

  @Test
  def testScanLeft {
    val in = 1 :: "2" :: HNil
    val out = in.scanLeft(1)(smear)

    typed[Int :: Int :: Int :: HNil](out)
    assertEquals(1 :: 2 :: 4 :: HNil, out)
  }

  @Test
  def testScanRight{
    val in = 1 :: "2" :: HNil
    val out = in.scanRight(1)(smear)

    typed[Int :: Int :: Int :: HNil](out)
    assertEquals(4 :: 3 :: 1 :: HNil, out)
  }

  @Test
  def testFill {
    {
      val empty = HList.fill(0)(true)
      typed[_0](empty.length)
    }

    {
      val empty = HList.fill[Boolean](0)(true)
      typed[_0](empty.length)
    }

    {
      val single = HList.fill(1)(None)
      typed[_1](single.length)
      typed[None.type](single.head)
      assertEquals(None, single.head)
    }

    {
      val single = HList.fill[None.type](1)(None)
      typed[_1](single.length)
      typed[None.type](single.head)
      assertEquals(None, single.head)
    }

    {
      val three = HList.fill(3)(m2i)
      typed[_3](three.length)
      typed[M2[Int, Unit]](three(_0))
      typed[M2[Int, Unit]](three(_1))
      typed[M2[Int, Unit]](three(_2))
      assertEquals(m2i, three(_0))
      assertEquals(m2i, three(_1))
      assertEquals(m2i, three(_2))
    }

    {
      val three = HList.fill[M2[Int, Unit]](3)(m2i)
      typed[_3](three.length)
      typed[M2[Int, Unit]](three(_0))
      typed[M2[Int, Unit]](three(_1))
      typed[M2[Int, Unit]](three(_2))
      assertEquals(m2i, three(_0))
      assertEquals(m2i, three(_1))
      assertEquals(m2i, three(_2))
    }

    {
      val empty = HList.fill(0, 0)(true)
      typed[_0](empty.length)
    }

    {
      val empty = HList.fill[Boolean](0, 0)(true)
      typed[_0](empty.length)
    }

    {
      val empty = HList.fill(2, 0)(true)
      typed[_2](empty.length)
      typed[_0](empty(_0).length)
      typed[_0](empty(_1).length)
    }

    {
      val empty = HList.fill[Boolean](2, 0)(true)
      typed[_2](empty.length)
      typed[_0](empty(_0).length)
      typed[_0](empty(_1).length)
    }

    {
      val empty = HList.fill(0, 2)(true)
      typed[_0](empty.length)
    }

    {
      val empty = HList.fill[Boolean](0, 2)(true)
      typed[_0](empty.length)
    }

    {
      val oneByTwo = HList.fill(1, 2)(None)
      typed[_1](oneByTwo.length)
      typed[_2](oneByTwo.head.length)
      typed[None.type](oneByTwo.head(_0))
      typed[None.type](oneByTwo.head(_1))
      assertEquals(None, oneByTwo.head(_0))
      assertEquals(None, oneByTwo.head(_1))
    }

    {
      val oneByTwo = HList.fill[None.type](1, 2)(None)
      typed[_1](oneByTwo.length)
      typed[_2](oneByTwo.head.length)
      typed[None.type](oneByTwo.head(_0))
      typed[None.type](oneByTwo.head(_1))
      assertEquals(None, oneByTwo.head(_0))
      assertEquals(None, oneByTwo.head(_1))
    }

    {
      val twoByThree = HList.fill(2, 3)(None)
      typed[_2](twoByThree.length)
      typed[_3](twoByThree(_0).length)
      typed[_3](twoByThree(_1).length)
      typed[None.type](twoByThree.at[_0].at[_0])
      typed[None.type](twoByThree.at[_0].at[_1])
      typed[None.type](twoByThree.at[_0].at[_2])
      typed[None.type](twoByThree.at[_1].at[_0])
      typed[None.type](twoByThree.at[_1].at[_1])
      typed[None.type](twoByThree.at[_1].at[_2])
      assertEquals(None, twoByThree.at[_0].at[_0])
      assertEquals(None, twoByThree.at[_0].at[_1])
      assertEquals(None, twoByThree.at[_0].at[_2])
      assertEquals(None, twoByThree.at[_1].at[_0])
      assertEquals(None, twoByThree.at[_1].at[_1])
      assertEquals(None, twoByThree.at[_1].at[_2])
    }

    {
      val twoByThree = HList.fill[None.type](2, 3)(None)
      typed[_2](twoByThree.length)
      typed[_3](twoByThree(_0).length)
      typed[_3](twoByThree(_1).length)
      typed[None.type](twoByThree.at[_0].at[_0])
      typed[None.type](twoByThree.at[_0].at[_1])
      typed[None.type](twoByThree.at[_0].at[_2])
      typed[None.type](twoByThree.at[_1].at[_0])
      typed[None.type](twoByThree.at[_1].at[_1])
      typed[None.type](twoByThree.at[_1].at[_2])
      assertEquals(None, twoByThree.at[_0].at[_0])
      assertEquals(None, twoByThree.at[_0].at[_1])
      assertEquals(None, twoByThree.at[_0].at[_2])
      assertEquals(None, twoByThree.at[_1].at[_0])
      assertEquals(None, twoByThree.at[_1].at[_1])
      assertEquals(None, twoByThree.at[_1].at[_2])
    }
  }

  @Test
  def testPatch {
    val basehl = 1 :: 2 :: "three" :: HNil

    { //patch an empty hlist
      val out = HNil.patch(0, basehl, 0)
      val out2 = HNil.patch[_0,_0](basehl)

      typed[Int :: Int :: String :: HNil](out)
      assertEquals(out, basehl)
      assertTypedEquals[Int :: Int :: String :: HNil](out, out2)
    }

    { //single patch w/ nothing removed
      val out = basehl.patch(1, 4 :: HNil, 0)
      val out2 = basehl.patch[_1,_0](4 :: HNil)

      typed[Int :: Int :: Int :: String :: HNil](out)
      assertEquals(1 :: 4 :: 2 :: "three" :: HNil, out)
      assertTypedEquals[Int :: Int :: Int :: String :: HNil](out, out2)
    }

    { //single patch w/ 2 elements removed
      val out = basehl.patch(1, 3 :: HNil, 2)
      val out2 = basehl.patch[_1,_2](3 :: HNil)

      typed[Int :: Int :: HNil](out)
      assertEquals(1 :: 3 :: HNil, out)
      assertTypedEquals[Int :: Int :: HNil](out, out2)
    }

    { //essentially append
      val p = 4 :: 5 :: "six" :: HNil
      val out = basehl.patch(3, p, 0)
      val out2 = basehl.patch[_3,_0](p)

      typed[Int :: Int :: String :: Int :: Int :: String :: HNil](out)
      assertEquals(1 :: 2 :: "three" :: 4 :: 5 :: "six" :: HNil, out)
      assertTypedEquals[Int :: Int :: String :: Int :: Int :: String :: HNil](out, out2)
    }

    { //several patched w/ everything from original removed
      val sub = 4 :: "five" :: "six" :: HNil
      val out = basehl.patch(0, sub, 3)
      val out2 = basehl.patch[_0,_3](sub)

      typed[Int :: String :: String :: HNil](out)
      assertEquals(sub, out)
      assertTypedEquals[Int :: String :: String :: HNil](out, out2)
    }
  }

  @Test
  def testToCoproduct {
    type PISB = Int :: String :: Boolean :: HNil
    type CISBa = Int :+: String :+: Boolean :+: CNil
    type CISBb = the.`ToCoproduct[PISB]`.Out
    implicitly[CISBa =:= CISBb]
  }
}
