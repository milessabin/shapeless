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
    
    typed[Int](l.head)
    assertEquals(1, l.head)
    
    typed[String](l.tail.head) 
    assertEquals("foo", l.tail.head)
    
    typed[Double](l.tail.tail.head)
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
    typed[OI](o1)
    assertEquals(Option(1) :: HNil, o1)

    val s2 = Set(1) :: Set("foo") :: HNil
    val o2 = s2 map choose
    typed[OIOS](o2)
    assertEquals(Option(1) :: Option("foo") :: HNil, o2)
    
    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val l2 = l1 map singleton
    typed[SISSSISI](l2)
    assertEquals(Set(1) :: Set("foo") :: Set(2) :: Set(3) :: HNil, l2)

    val l3 = l1 map option
    typed[OIOSOIOI](l3)
    assertEquals(Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil, l3)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    
    val l5 = l4 map get
    typed[ISII](l5)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, l5)
    
    typed[Int](l5.head)
    typed[String](l5.tail.head)
    typed[Int](l5.tail.tail.head)
    typed[Int](l5.tail.tail.tail.head)
    
    val l6 = l1 map identity
    typed[ISII](l6)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, l6)

    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals(true :: true :: true :: true :: HNil, l7)
    
    val l8 = 23 :: "foo" :: true :: HNil
    val l9 = l8 map mkString
    typed[String :: String :: String :: HNil](l9)
    assertEquals("23" :: "foo" :: "true" :: HNil, l9)
    
    val l10 = apbp map fruit
    typed[Fruit :: Fruit :: Fruit :: Fruit :: HNil](l10)
    assertEquals(apbp, l10)
    
    val l11 = apbp map mkString
    typed[String :: String :: String :: String :: HNil](l11)
    assertEquals("Apple()" :: "Pear()" :: "Banana()" :: "Pear()" :: HNil, l11)
  }

  object dup extends Poly1 {
    implicit def default[T] = at[T](t => t :: t :: HNil)
  }

  @Test
  def testFlatMap {
    val l1 = 1 :: "foo" :: true :: HNil

    val l2 = l1 flatMap dup
    typed[Int :: Int :: String :: String :: Boolean :: Boolean :: HNil](l2)
    assertEquals(1 :: 1 :: "foo" :: "foo" :: true :: true :: HNil, l2)

    val l3 = (1 :: "foo" :: HNil) :: (HNil : HNil) :: (2.0 :: true :: HNil) :: ("bar" :: HNil) :: HNil

    val l4 = l3 flatMap identity
    typed[Int :: String :: Double :: Boolean :: String :: HNil](l4)
    assertEquals(1 :: "foo" :: 2.0 :: true :: "bar" :: HNil, l4)
    
    val l5 = 23 :: "foo" :: 7 :: true :: 0 :: HNil
    val l6 = l5 flatMap incInt
    typed[Int :: Int :: Int :: HNil](l6)
    assertEquals(24 :: 8 :: 1 :: HNil, l6)
    
    val l7 = Set(23) :: "foo" :: Set(true) :: 23 :: HNil
    val l8 = l7 flatMap extendedChoose
    typed[Option[Int] :: Option[Boolean] :: HNil](l8)
    assertEquals(Option(23) :: Option(true) :: HNil, l8)
  }
  
  @Test
  def testConformance {
    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil
    typed[Any :: AnyRef :: Any :: Any :: HNil](l1)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, l1)

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
    typed[Pear](lp)
    assertEquals(p, lp)
    
    val iapb = apbp.init
    typed[APB](iapb)
    assertEquals(a :: p :: b :: HNil, iapb)
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
    typed[M0](a0)
    assertEquals(23 :: "foo" :: true :: HNil, a0)

    val a1 = l.align(m1)
    typed[M1](a1)
    assertEquals(23 :: true :: "foo" :: HNil, a1)

    val a2 = l.align(m2)
    typed[M2](a2)
    assertEquals("foo" :: 23 :: true :: HNil, a2)

    val a3 = l.align(m3)
    typed[M3](a3)
    assertEquals("foo" :: true :: 23 :: HNil, a3)

    val a4 = l.align(m4)
    typed[M4](a4)
    assertEquals(true :: 23 :: "foo" :: HNil, a4)

    val a5 = l.align(m5)
    typed[M5](a5)
    assertEquals(true :: "foo" :: 23 :: HNil, a5)

    val b0 = l.align[M0]
    typed[M0](b0)
    assertEquals(23 :: "foo" :: true :: HNil, b0)

    val b1 = l.align[M1]
    typed[M1](b1)
    assertEquals(23 :: true :: "foo" :: HNil, b1)

    val b2 = l.align[M2]
    typed[M2](b2)
    assertEquals("foo" :: 23 :: true :: HNil, b2)

    val b3 = l.align[M3]
    typed[M3](b3)
    assertEquals("foo" :: true :: 23 :: HNil, b3)

    val b4 = l.align[M4]
    typed[M4](b4)
    assertEquals(true :: 23 :: "foo" :: HNil, b4)

    val b5 = l.align[M5]
    typed[M5](b5)
    assertEquals(true :: "foo" :: 23 :: HNil, b5)

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
    typed[PBPA](pbpa)
    assertEquals(p :: b :: p :: a :: HNil, pbpa)

    val al = a :: HNil
    val ral = al.reverse
    typed[Apple :: HNil](ral)
    assertEquals(a :: HNil, ral)
  }
    
  @Test
  def testPrepend {
    val apbp2 = ap ::: bp
    typed[APBP](apbp2)
    assertEquals(a :: p :: b :: p :: HNil, apbp2)

    typed[Apple](apbp2.head)
    typed[Pear](apbp2.tail.head)
    typed[Banana](apbp2.tail.tail.head)
    typed[Pear](apbp2.tail.tail.tail.head)
    
    val pabp = ap reverse_::: bp
    typed[PABP](pabp)
    assertEquals(p :: a :: b :: p :: HNil, pabp)

    // must compile without requiring an implicit Prepend
    def prependWithHNil[L <: HList](list: L) = HNil ::: list
    def prependToHNil[L <: HList](list: L) = list ::: HNil
    assertEquals(prependWithHNil(ap), ap)
    assertEquals(prependToHNil(ap), ap)
    assertEquals(HNil ::: HNil, HNil)

    // must compile without requiring an implicit ReversePrepend
    def reversePrependWithHNil[L <: HList](list: L) = HNil reverse_::: list
    def reversePrependToHNil[L <: HList: Reverse](list: L) = list reverse_::: HNil
    assertEquals(reversePrependWithHNil(ap), ap)
    assertEquals(reversePrependToHNil(ap), ap.reverse)
    assertEquals(HNil reverse_::: HNil, HNil)
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
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr1.asInstanceOf[Array[Object]])

    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

    val hnil = HNil
    val snil = hnil.toSized[Array]
    assertEquals(Nat toInt hnil.length, snil.length)
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
    val emptySizedList = HNil.to[List]
    typed[List[Nothing]](emptySizedList)
    assertEquals(Nil, emptySizedList)

    val fruits1 = apap.to[List]
    typed[List[Fruit]](fruits1)
    assertEquals(List(a, p, a, p), fruits1)

    val fruits2 = apbp.to[List]
    typed[List[Fruit]](fruits2)
    assertEquals(List(a, p, b, p), fruits2)

    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    typed[APBP](fruits3.get)
    assertEquals(apbp, fruits3.get)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.to[List]
    typed[List[Any]](stuff)
    assertEquals(List(1, "foo", 2, 3), stuff)

    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    typed[ISII](stuff2.get)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals(true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.to[List]
    typed[Boolean](ll2.head)

    val moreStuff = (a :: "foo" :: p :: HNil).to[List]
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
  def testToList {
    val emptySizedList = HNil.toList
    typed[List[Nothing]](emptySizedList)
    assertEquals(Nil, emptySizedList)

    val fruits1 = apap.toList
    typed[List[Fruit]](fruits1)
    assertEquals(List(a, p, a, p), fruits1)
    
    val fruits2 = apbp.toList
    typed[List[Fruit]](fruits2)
    assertEquals(List(a, p, b, p), fruits2)
    
    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    typed[APBP](fruits3.get)
    assertEquals(apbp, fruits3.get)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.toList
    typed[List[Any]](stuff)
    assertEquals(List(1, "foo", 2, 3), stuff)
    
    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    typed[ISII](stuff2.get)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals(true :: true :: true :: true :: HNil, l7)

    val ll2 = l7.toList
    typed[Boolean](ll2.head)

    val moreStuff = (a :: "foo" :: p :: HNil).toList
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

    // With existentials, it gets more tricky
    val mWithEx = mimsmimemd.toList
    // Compiler fails complaining that it 
    //    Cannot prove that List[HListTests.this.M[_ >: Double with _$1 with Int with String]] =:= List[HListTests.this.M[_]]
    //  equalType(mimsmimemdList, mWithEx)
    typed[List[M[_]]](mWithEx)
    assertEquals(mimsmimemdList, mWithEx)

    // Second order higher kinded types are ok...
    val m2 = m2im2sm2im2im2d.toList
    equalInferredTypes(m2im2sm2im2im2dList, m2)
    typed[List[M2[_ >: Int with String with Double, Unit]]](m2)
    assertEquals(m2im2sm2im2im2dList, m2)

    // ...as long as existentials are not involved. 
    val m2e = m2eim2esm2eim2eem2ed.toList
    // Compiler complains that it
    //    Cannot prove that List[HListTests.this.M2[_ >: Double with Int with Int with String with Int, _ >: _$5 with _$3 with _$3 with _$4 with _$3]] =:= List[HListTests.this.M2[_35,_36] forSome { type _$10; type _$9; type _34 >: _$10 with _$9; type _$8; type _$7; type _32 >: _$8 with _$7; type _35 >: Double with Int with Int with String; type _36 >: _34 with _32 }]
    // equalType(m2eim2esm2eim2eem2edList, m2e)
    typed[List[M2[_ >: Int with String with Double, _]]](m2e)
    assertEquals(m2eim2esm2eim2eem2edList, m2e)
  }
  
  @Test
  def testToTraversableArray {
    def assertArrayEquals2[T](arr1 : Array[T], arr2 : Array[T]) =
      assertArrayEquals(arr1.asInstanceOf[Array[Object]], arr1.asInstanceOf[Array[Object]])

    val empty = HNil.to[Array]
    typed[Array[Nothing]](empty)
    assertArrayEquals2(Array[Nothing](), empty)

    val fruits1 = apap.to[Array].map(x => x : Fruit) // Default inferred type is too precise 
                                                     // (Product with Serializable with Fruit)
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)

    val fruits2 = apbp.to[Array].map(x => x : Fruit)
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)

    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    typed[APBP](fruits3.get)
    assertEquals(apbp, fruits3.get)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.to[Array]
    typed[Array[Any]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)

    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    typed[ISII](stuff2.get)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals(true :: true :: true :: true :: HNil, l7)

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

    val fruits1 = apap.toArray[Fruit]
    typed[Array[Fruit]](fruits1)
    assertArrayEquals2(Array[Fruit](a, p, a, p), fruits1)
    
    val fruits2 = apbp.toArray[Fruit]
    typed[Array[Fruit]](fruits2)
    assertArrayEquals2(Array[Fruit](a, p, b, p), fruits2)
    
    val fruits3 = fruits2.toHList[APBP]
    assertTrue(fruits3.isDefined)
    typed[APBP](fruits3.get)
    assertEquals(apbp, fruits3.get)

    val l1 = 1 :: "foo" :: 2 :: 3 :: HNil

    val stuff = l1.toArray
    typed[Array[Any]](stuff)
    assertArrayEquals2(Array(1, "foo", 2, 3), stuff)
    
    val stuff2 = stuff.toHList[ISII]
    assertTrue(stuff2.isDefined)
    typed[ISII](stuff2.get)
    assertEquals(1 :: "foo" :: 2 :: 3 :: HNil, stuff2.get)

    val l4 = Option(1) :: Option("foo") :: Option(2) :: Option(3) :: HNil
    val l7 = l4 map isDefined
    typed[BBBB](l7)
    assertEquals(true :: true :: true :: true :: HNil, l7)

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
    
    val sn2 =
      0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 
      10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 
      20 :: 21 :: 22 :: HNil
      
    val at22 = sn2(_22)
    typed[Int](at22)
    assertEquals(22, at22)
  }
  
  @Test
  def testAtLiteral {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil
    
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
    
    val sn2 =
      0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 
      10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 
      20 :: 21 :: 22 :: HNil
      
    val at22 = sn2(22)
    typed[Int](at22)
    assertEquals(22, at22)
  }
  
  @Test
  def testTakeDrop {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil
    
    val t0 = sn1.take(_0)
    typed[HNil](t0)
    assertEquals(HNil, t0)
    
    val d0 = sn1.drop(_0)
    typed[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](d0)
    assertEquals(23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, d0)

    val t2 = sn1.take(_2)
    typed[Int :: Double :: HNil](t2)
    assertEquals(23 :: 3.0 :: HNil, t2)
    
    val d2 = sn1.drop(_2)
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](d2)
    assertEquals("foo" :: () :: "bar" :: true :: 5L :: HNil, d2)

    val t7 = sn1.take(_7)
    typed[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](t7)
    assertEquals(23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, t7)
    
    val d7 = sn1.drop(_7)
    typed[HNil](d7)
    assertEquals(HNil, d7)
  }
  
  @Test
  def testTakeDropLiteral {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil
    
    val t0 = sn1.take(0)
    typed[HNil](t0)
    assertEquals(HNil, t0)
    
    val d0 = sn1.drop(0)
    typed[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](d0)
    assertEquals(23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, d0)

    val t2 = sn1.take(2)
    typed[Int :: Double :: HNil](t2)
    assertEquals(23 :: 3.0 :: HNil, t2)
    
    val d2 = sn1.drop(2)
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](d2)
    assertEquals("foo" :: () :: "bar" :: true :: 5L :: HNil, d2)

    val t7 = sn1.take(7)
    typed[Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil](t7)
    assertEquals(23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil, t7)
    
    val d7 = sn1.drop(7)
    typed[HNil](d7)
    assertEquals(HNil, d7)
  }
  
  @Test
  def testSplit {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.split(_0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni0)
    val sni1 = sn1.split(_1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni1)
    val sni2 = sn1.split(_2)
    typed[(Int :: Double :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](sni2)
    val sni3 = sn1.split(_3)
    typed[(Int :: Double :: String :: HNil, Unit :: String :: Boolean :: Long :: HNil)](sni3)
    val sni4 = sn1.split(_4)
    typed[(Int :: Double :: String :: Unit :: HNil, String :: Boolean :: Long :: HNil)](sni4)
    val sni5 = sn1.split(_5)
    typed[(Int :: Double :: String :: Unit :: String :: HNil, Boolean :: Long :: HNil)](sni5)
    val sni6 = sn1.split(_6)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: HNil, Long :: HNil)](sni6)
    val sni7 = sn1.split(_7)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil, HNil)](sni7)
    
    val snri0 = sn1.reverse_split(_0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri0)
    val snri1 = sn1.reverse_split(_1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri1)
    val snri2 = sn1.reverse_split(_2)
    typed[(Double :: Int :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](snri2)
    val snri3 = sn1.reverse_split(_3)
    typed[(String :: Double :: Int :: HNil, Unit :: String :: Boolean :: Long :: HNil)](snri3)
    val snri4 = sn1.reverse_split(_4)
    typed[(Unit :: String :: Double :: Int :: HNil, String :: Boolean :: Long :: HNil)](snri4)
    val snri5 = sn1.reverse_split(_5)
    typed[(String :: Unit :: String :: Double :: Int :: HNil, Boolean :: Long :: HNil)](snri5)
    val snri6 = sn1.reverse_split(_6)
    typed[(Boolean :: String :: Unit :: String :: Double :: Int :: HNil, Long :: HNil)](snri6)
    val snri7 = sn1.reverse_split(_7)
    typed[(Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil, HNil)](snri7)
  }
  
  @Test
  def testSplitLiteral {
    val sn1 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val sni0 = sn1.split(0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni0)
    val sni1 = sn1.split(1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](sni1)
    val sni2 = sn1.split(2)
    typed[(Int :: Double :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](sni2)
    val sni3 = sn1.split(3)
    typed[(Int :: Double :: String :: HNil, Unit :: String :: Boolean :: Long :: HNil)](sni3)
    val sni4 = sn1.split(4)
    typed[(Int :: Double :: String :: Unit :: HNil, String :: Boolean :: Long :: HNil)](sni4)
    val sni5 = sn1.split(5)
    typed[(Int :: Double :: String :: Unit :: String :: HNil, Boolean :: Long :: HNil)](sni5)
    val sni6 = sn1.split(6)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: HNil, Long :: HNil)](sni6)
    val sni7 = sn1.split(7)
    typed[(Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil, HNil)](sni7)
    
    val snri0 = sn1.reverse_split(0)
    typed[(HNil, Int :: Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri0)
    val snri1 = sn1.reverse_split(1)
    typed[(Int :: HNil, Double :: String :: Unit :: String :: Boolean :: Long :: HNil)](snri1)
    val snri2 = sn1.reverse_split(2)
    typed[(Double :: Int :: HNil, String :: Unit :: String :: Boolean :: Long :: HNil)](snri2)
    val snri3 = sn1.reverse_split(3)
    typed[(String :: Double :: Int :: HNil, Unit :: String :: Boolean :: Long :: HNil)](snri3)
    val snri4 = sn1.reverse_split(4)
    typed[(Unit :: String :: Double :: Int :: HNil, String :: Boolean :: Long :: HNil)](snri4)
    val snri5 = sn1.reverse_split(5)
    typed[(String :: Unit :: String :: Double :: Int :: HNil, Boolean :: Long :: HNil)](snri5)
    val snri6 = sn1.reverse_split(6)
    typed[(Boolean :: String :: Unit :: String :: Double :: Int :: HNil, Long :: HNil)](snri6)
    val snri7 = sn1.reverse_split(7)
    typed[(Long :: Boolean :: String :: Unit :: String :: Double :: Int :: HNil, HNil)](snri7)
  }
  
  @Test
  def testSelect {
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
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
    val l1 = 1 :: 2 :: HNil
    val f1 = l1.filter[Int]
    typed[Int :: Int :: HNil](f1)
    assertEquals(1 :: 2 :: HNil, f1)

    val l2 = 1 :: true :: "foo" :: 2 :: HNil
    val f2 = l2.filter[Int]
    typed[Int :: Int :: HNil](f2)
    assertEquals(1 :: 2 :: HNil, f2)

    typed[HNil](l2.filter[Double])
  }

  @Test
  def testFilterNot {
    val l1 = 1 :: 2 :: HNil
    val f1 = l1.filterNot[String]
    typed[Int :: Int :: HNil](f1)
    assertEquals(1 :: 2 :: HNil, f1)

    val l2 = 1 :: true :: "foo" :: 2 :: HNil
    val f2 = l2.filterNot[String]
    typed[Int :: Boolean :: Int :: HNil](f2)
    assertEquals(1 :: true :: 2 :: HNil, f2)

    typed[HNil](l2.filter[Double])
  }

  @Test
  def testReplace {
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
    
    val (i, r1) = sl.replace(23)
    typed[Int](i)
    assertEquals(1, i)
    assertEquals(23 :: true :: "foo" :: 2.0 :: HNil, r1)
    
    val (b, r2) = sl.replace(false)
    typed[Boolean](b)
    assertEquals(true, b)
    assertEquals(1 :: false :: "foo" :: 2.0 :: HNil, r2)

    val (s, r3) = sl.replace("bar")
    typed[String](s)
    assertEquals("foo", s)
    assertEquals(1 :: true :: "bar" :: 2.0 :: HNil, r3)

    val (d, r4) = sl.replace(3.0)
    typed[Double](d)
    assertEquals(2.0, d, Double.MinPositiveValue)
    assertEquals(1 :: true :: "foo" :: 3.0 :: HNil, r4)
    
    val (i2, r5) = sl.replaceType[Int]('*')
    typed[Int](i2)
    typed[Char](r5(0))
    assertEquals(1, i2)
    assertEquals('*' :: true :: "foo" :: 2.0 :: HNil, r5)

    val (b2, r6) = sl.replaceType[Boolean]('*')
    typed[Boolean](b2)
    typed[Char](r6(1))
    assertEquals(true, b2)
    assertEquals(1 :: '*' :: "foo" :: 2.0 :: HNil, r6)

    val (s2, r7) = sl.replaceType[String]('*')
    typed[String](s2)
    typed[Char](r7(2))
    assertEquals("foo", s2)
    assertEquals(1 :: true :: '*' :: 2.0 :: HNil, r7)

    val (d2, r8) = sl.replaceType[Double]('*')
    typed[Double](d2)
    typed[Char](r8(3))
    assertEquals(2.0, d2, Double.MinPositiveValue)
    assertEquals(1 :: true :: "foo" :: '*' :: HNil, r8)
    
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
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
    
    val r1 = sl.updatedElem(23)
    assertEquals(23 :: true :: "foo" :: 2.0 :: HNil, r1)
    
    val r2 = sl.updatedElem(false)
    assertEquals(1 :: false :: "foo" :: 2.0 :: HNil, r2)

    val r3 = sl.updatedElem("bar")
    assertEquals(1 :: true :: "bar" :: 2.0 :: HNil, r3)

    val r4 = sl.updatedElem(3.0)
    assertEquals(1 :: true :: "foo" :: 3.0 :: HNil, r4)
    
    val r5 = sl.updatedType[Int]('*')
    assertEquals('*' :: true :: "foo" :: 2.0 :: HNil, r5)

    val r6 = sl.updatedType[Boolean]('*')
    assertEquals(1 :: '*' :: "foo" :: 2.0 :: HNil, r6)

    val r7 = sl.updatedType[String]('*')
    assertEquals(1 :: true :: '*' :: 2.0 :: HNil, r7)

    val r8 = sl.updatedType[Double]('*')
    assertEquals(1 :: true :: "foo" :: '*' :: HNil, r8)
    
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
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
    val sl2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil
    
    val (sp1, sp2) = sl.splitLeft[String]
    typed[Int :: Boolean :: HNil](sp1)
    typed[String :: Double :: HNil](sp2)
    assertEquals((sp1 ::: sp2), sl)

    val (sli1, sli2) = sl2.splitLeft[String]
    typed[Int :: Double :: HNil](sli1) 
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](sli2)
    assertEquals((sli1 ::: sli2), sl2)

    val (rsp1, rsp2) = sl.reverse_splitLeft[String]
    typed[Boolean :: Int :: HNil](rsp1)
    typed[String :: Double :: HNil](rsp2)
    assertEquals((rsp1 reverse_::: rsp2), sl)

    val (rsli1, rsli2) = sl2.reverse_splitLeft[String]
    typed[Double :: Int :: HNil](rsli1) 
    typed[String :: Unit :: String :: Boolean :: Long :: HNil](rsli2)
    assertEquals((rsli1 reverse_::: rsli2), sl2)

  }
  
  @Test
  def testSplitRight {
    val sl = 1 :: true :: "foo" :: 2.0 :: HNil
    val sl2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil

    val (srp1, srp2) = sl.splitRight[String]
    typed[Int :: Boolean :: String :: HNil](srp1)
    typed[Double :: HNil](srp2)
    assertEquals((srp1 ::: srp2), sl)

    val (srli1, srli2) = sl2.splitRight[String]
    typed[Int :: Double :: String :: Unit :: String :: HNil](srli1) 
    typed[Boolean :: Long :: HNil](srli2)
    assertEquals(sl2, srli1 ::: srli2)

    val (rsrp1, rsrp2) = sl.reverse_splitRight[String]
    typed[String :: Boolean :: Int :: HNil](rsrp1)
    typed[Double :: HNil](rsrp2)
    assertEquals((rsrp1 reverse_::: rsrp2), sl)

    val (rsrli1, rsrli2) = sl2.reverse_splitRight[String]
    typed[String :: Unit :: String :: Double :: Int :: HNil](rsrli1) 
    typed[Boolean :: Long :: HNil](rsrli2)
    assertEquals((rsrli1 reverse_::: rsrli2), sl2)
  }
  
  @Test
  def testTranspose {
    val l1 = 1 :: HNil
    val l2 = ("a" :: HNil) :: HNil
    
    val z1 = l1.zipOne(l2)
    typed[(Int :: String :: HNil) :: HNil](z1)
    assertEquals((1 :: "a" :: HNil) :: HNil, z1)
    
    val mc1 = l1.mapConst(HNil)
    typed[HNil :: HNil](mc1)
    assertEquals(HNil :: HNil, mc1)
    
    val t1 = (l1 :: HNil).transpose
    typed[(Int :: HNil) :: HNil](t1)
    assertEquals((1 :: HNil) :: HNil, t1)

    val l3 = 1 :: 2 :: 3 :: HNil
    val l4 = ("a" :: 1.0 :: HNil) :: ("b" :: 2.0 :: HNil) :: ("c" :: 3.0 :: HNil) :: HNil
    
    val z2 = l3.zipOne(l4)
    typed[(Int :: String :: Double :: HNil) :: (Int :: String :: Double :: HNil) :: (Int :: String :: Double :: HNil) :: HNil](z2)
    assertEquals((1 :: "a" :: 1.0 :: HNil) :: (2 :: "b" :: 2.0 :: HNil) :: (3 :: "c" :: 3.0 :: HNil) :: HNil, z2)
    
    val mc2 = l3.mapConst(HNil)
    typed[HNil :: HNil :: HNil :: HNil](mc2)
    assertEquals(HNil :: HNil :: HNil :: HNil, mc2)
    
    val t2 = l4.transpose
    typed[(String :: String :: String :: HNil) :: (Double :: Double :: Double :: HNil) :: HNil](t2)
    assertEquals(("a" :: "b" :: "c" :: HNil) :: (1.0 :: 2.0 :: 3.0 :: HNil) :: HNil, t2)
    
    val t3 = z2.transpose
    typed[(Int :: Int :: Int :: HNil) :: (String :: String :: String :: HNil) :: (Double :: Double :: Double :: HNil) :: HNil](t3)
    assertEquals((1 :: 2 :: 3 :: HNil) :: ("a" :: "b" :: "c" :: HNil) :: (1.0 :: 2.0 :: 3.0 :: HNil) :: HNil, t3)
    
    val t4 = t3.transpose
    typed[(Int :: String :: Double :: HNil) :: (Int :: String :: Double :: HNil) :: (Int :: String :: Double :: HNil) :: HNil](t4)
    assertEquals(z2, t4)
  }
  
  @Test
  def testZipUnzip {
    val l1 = 1 :: "a" :: 1.0 :: HNil
    val l2 = 2 :: "b" :: 2.0 :: HNil
    
    val t1 = (l1 :: l2 :: HNil).transpose
    val z1 = t1.map(tupled)
    typed[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](z1)
    assertEquals((1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z1)
    
    def zip[L <: HList, OutT <: HList](l : L)
      (implicit
        transposer : Transposer.Aux[L, OutT],
        mapper : Mapper[tupled.type, OutT]) = l.transpose.map(tupled)
    
    val z2 = zip(l1 :: l2 :: HNil)
    typed[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](z2)
    assertEquals((1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z2)

    val z3 = (l1 :: l2 :: HNil).zip
    typed[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](z3)
    assertEquals((1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z3)
    
    val t2 = z1.map(productElements).transpose
    val u1 = t2.tupled
    typed[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](u1)
    assertEquals((1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), u1)

    def unzip[L <: HList, OutM <: HList, OutT <: HList](l : L)
      (implicit
        mapper : Mapper.Aux[productElements.type, L, OutM],
        transposer : Transposer.Aux[OutM, OutT],
        tupler : Tupler[OutT]) = l.map(productElements).transpose.tupled
        
    val u2 = unzip(z1)
    typed[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](u2)
    assertEquals((1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), u2)
    
    val u3 = z1.unzip
    typed[(Int :: String :: Double :: HNil, Int :: String :: Double :: HNil)](u3)
    assertEquals((1 :: "a" :: 1.0 :: HNil, 2 :: "b" :: 2.0 :: HNil), u3)
    
    val z4 = l1 zip l2
    typed[(Int, Int) :: (String, String) :: (Double, Double) :: HNil](z4)
    assertEquals((1, 2) :: ("a", "b") :: (1.0, 2.0) :: HNil, z4)

    val intInc : Int => Int = _+1
    val stringInc : String => String = _+"*"
    val doubleInc : Double => Int = _.toInt+1
    
    val l3 = intInc :: stringInc :: doubleInc :: HNil
    
    val z5 = l3 zipApply l1
    typed[Int :: String :: Int :: HNil](z5)
    assertEquals(2 :: "a*" :: 2 :: HNil, z5)
  }
  
  @Test
  def testUnapply {
    val l = 1 :: true :: "foo" :: 2.0 :: HNil
    val l2 = 23 :: 3.0 :: "foo" :: () :: "bar" :: true :: 5L :: HNil
    
    val is = l match {
      case i :: true :: s :: 2.0 :: HNil => (i, s) 
    }
    
    typed[(Int, String)](is)
    assertEquals(1, is._1)
    assertEquals("foo", is._2)

    val is2 = (l : Any) match {
      case (i : Int) :: true :: (s : String) :: 2.0 :: HNil => (i, s)
      case _ => sys.error("Not matched")
    }

    typed[(Int, String)](is2)
    assertEquals(1, is2._1)
    assertEquals("foo", is2._2)
    
    import HList.ListCompat._
    
    val tl = l2 match {
      case 23 #: 3.0 #: s #: xs => (s, xs)
    }
    
    typed[(String, Unit :: String :: Boolean :: Long :: HNil)](tl)
    assertEquals("foo", tl._1)
    assertEquals(() :: "bar" :: true :: 5L :: HNil, tl._2)

    val tl2 = (l2 : Any) match {
      case 23 #: 3.0 #: (s : String) #: xs => (s, xs)
      case _ => sys.error("Not matched")
    }
    
    typed[(String, HList)](tl2)
    assertEquals("foo", tl2._1)
    assertEquals(() :: "bar" :: true :: 5L :: HNil, tl2._2)
    
    val ll = List(1, 2, 3, 4)
    val tll = ll match {
      case 1 :: 2 :: x :: y :: Nil => (x, y)
      case _ => sys.error("Not matched")
    }
    typed[(Int, Int)](tll)
    assertEquals(3, tll._1)
    assertEquals(4, tll._2)
    
    val tll2 = ll match {
      case 1 :: xs => xs
      case _ => sys.error("Not matched")
    }
    typed[List[Int]](tll2)
    assertEquals(List(2, 3, 4), tll2)
    
    val mixed = 23 :: "foo" :: (1 :: 2 :: 3 :: 4 :: 5 :: Nil) :: false :: () :: HNil
    val tmixed = mixed match {
      case _ #: _ #: (_ :: 2 :: x :: tl1) #: tl2 => (x, tl1, tl2) 
      case _ => sys.error("Not matched")
    }
    typed[(Int, List[Int], Boolean :: Unit :: HNil)](tmixed)
    assertEquals(3, tmixed._1)
    assertEquals(4 :: 5 :: Nil, tmixed._2)
    assertEquals(false :: () :: HNil, tmixed._3)
  }

  @Test
  def testRemove {
    val l = 1 :: true :: "foo" :: HNil

    val li = l.removeElem[Int]
    typed[(Int, Boolean :: String :: HNil)](li)
    assertEquals((1, true :: "foo" :: HNil), li)

    val lb = l.removeElem[Boolean]
    typed[(Boolean, Int :: String :: HNil)](lb)
    assertEquals((true, 1 :: "foo" :: HNil), lb)

    val ls = l.removeElem[String]
    typed[(String, Int :: Boolean :: HNil)](ls)
    assertEquals(("foo", 1 :: true :: HNil), ls)
  }

  @Test
  def testRemoveAll {
    val l = 1 :: true :: "foo" :: HNil
    
    val lnil = l.removeAll[HNil]
    typed[(HNil, Int :: Boolean :: String :: HNil)](lnil)
    assertEquals((HNil, 1 :: true :: "foo" :: HNil), lnil)

    val li = l.removeAll[Int :: HNil]
    typed[(Int :: HNil, Boolean :: String :: HNil)](li)
    assertEquals((1 :: HNil, true :: "foo" :: HNil), li)

    val lb = l.removeAll[Boolean :: HNil]
    typed[(Boolean :: HNil, Int :: String :: HNil)](lb)
    assertEquals((true :: HNil, 1 :: "foo" :: HNil), lb)

    val lbi = l.removeAll[Boolean :: Int :: HNil]
    typed[(Boolean :: Int :: HNil, String :: HNil)](lbi)
    assertEquals((true :: 1 :: HNil, "foo" :: HNil), lbi)
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
    
    implicitly[LeftFolder.Aux[HNil, String, combine.type, String]]
    implicitly[LeftFolder.Aux[Boolean :: HNil, Int, combine.type, String]]
    implicitly[LeftFolder.Aux[String :: Boolean :: HNil, Char, combine.type, String]]

    val tf1 = implicitly[LeftFolder[HNil, String, combine.type]]
    val tf2 = implicitly[LeftFolder[Boolean :: HNil, Int, combine.type]]
    val tf3 = implicitly[LeftFolder[String :: Boolean :: HNil, Char, combine.type]]

    val l1 = "foo" :: true :: HNil
    val f1 = l1.foldLeft('o')(combine)
    typed[String](f1)
    assertEquals("pass", f1)

    val c2a = combine('o', "bar")
    val c2b = combine(c2a, false)
    assertEquals("pass", c2b)

    val l2 = "bar" :: false :: HNil
    val f2 = l2.foldLeft('o')(combine)
    typed[String](f2)
    assertEquals("pass", f2)
  }
  
  @Test
  def testUpdatedAt {
    type IBS = Int :: Boolean :: String :: HNil
    
    val l = 1 :: true :: "foo" :: HNil

    val li = l.updatedAt[_0](2)
    typed[IBS](li)
    assertEquals(2 :: true :: "foo" :: HNil, li)

    val lb = l.updatedAt[_1](false)
    typed[IBS](lb)
    assertEquals(1 :: false :: "foo" :: HNil, lb)

    val ls = l.updatedAt[_2]("bar")
    typed[IBS](ls)
    assertEquals(1 :: true :: "bar" :: HNil, ls)
  }

  @Test
  def testUpdatedAtLiteral {
    type IBS = Int :: Boolean :: String :: HNil
    
    val l = 1 :: true :: "foo" :: HNil

    val li = l.updatedAt(0, 2)
    typed[IBS](li)
    assertEquals(2 :: true :: "foo" :: HNil, li)

    val lb = l.updatedAt(1, false)
    typed[IBS](lb)
    assertEquals(1 :: false :: "foo" :: HNil, lb)

    val ls = l.updatedAt(2, "bar")
    typed[IBS](ls)
    assertEquals(1 :: true :: "bar" :: HNil, ls)
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

    assertEquals(l2, List(1, 1, 1) :: List(true, true, true) :: List("three", "three", "three") :: List() :: HNil)
  }

  @Test
  def testZipConst {
    type IBS = Int :: Boolean :: String :: HNil
    val c = 5
    type WithConst = (Int, Int) :: (Boolean, Int) :: (String, Int) :: HNil
    val l = 1 :: true :: "a" :: HNil
    typed[IBS](l)
    val expected = (1, c) :: (true, c) :: ("a", c) :: HNil
    typed[WithConst](expected)

    val zcIntIbs = implicitly[ZipConst[Int, IBS]]
    val zipped1 = zcIntIbs(c, l)
    assertEquals(expected, zipped1)

    val zcaIntIbs = implicitly[ZipConst.Aux[Int, IBS, WithConst]]
    val zipped2 = zcaIntIbs(c, l)
    typed[WithConst](zipped2)
    assertEquals(expected, zipped2)

    val zipped3 = l.zipConst(c)
    typed[WithConst](zipped3)
    assertEquals(expected, zipped3)
  }

  @Test
  def testZipWith {
    import poly._

    { // HNil zipWith HNil (emptyFn)
      object zipFn extends Poly2

      val left: HNil = HNil
      val right: HNil = HNil
      val result = left.zipWith(right)(zipFn)
      typed[HNil](result)
      assertEquals(HNil, result)
    }

    { // HNil zipWith nonEmpty (emptyFn)
      object zipFn extends Poly2

      val left: HNil = HNil
      val right = 1 :: HNil
      val result = left.zipWith(right)(zipFn)
      typed[HNil](result)
      assertEquals(HNil, result)
    }

    { // nonEmpty zipWith HNil (emptyFn)
      object zipFn extends Poly2

      val left = 1 :: HNil
      val right: HNil = HNil
      val result = left.zipWith(right)(zipFn)
      typed[HNil](result)
      assertEquals(HNil, result)
    }

    { // singleton zipWith singleton
      object zipFn extends Poly2 {
        implicit val caseIntInt = at[Int, Int](_ + _)
      }

      val left  = 1 :: HNil
      val right = 2 :: HNil
      val result = left.zipWith(right)(zipFn)
      typed[Int :: HNil](result)
      assertEquals(3 :: HNil, result)
    }

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

      val result = left.zipWith(right)(zipFn)
      typed[Int :: String :: Double :: HNil](result)

      assertEquals(3 :: "foo -> 2.3" :: 4.6 :: HNil, result)
    }

    { // invalid polys
      object emptyFn extends Poly2

      illTyped("""
        (1 :: HNil).zipWith(2 :: HNil)(emptyFn)
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
    import syntax.singleton._

    val orig =
      ("intField" ->> 1) ::
      ("boolField" ->> true) ::
      HNil

    val result = orig.values.zipWithKeys(orig.keys)
    sameTyped(orig)(result)
    assertEquals(orig, result)
    val int = result.get("intField")
    typed[Int](int)
    assertEquals(1, int)
    val bool = result.get("boolField")
    typed[Boolean](bool)
    assertEquals(true, bool)
    illTyped("""result.get("otherField")""")

    // key/value lengths must match up
    illTyped("orig.tail.values.zipWithKeys(orig.keys)")
    illTyped("orig.values.zipWithKeys(orig.keys.tail)")
  }

  @Test
  def testCollect {
    import poly._

    object empty extends Poly1

    object complex extends Poly1 {
      implicit val caseInt    = at[Int](_.toDouble)
      implicit val caseString = at[String](_ => 1)
    }

    { // HNil collect p
      val in: HNil = HNil

      val emptyResult = in.collect(empty)
      typed[HNil](emptyResult)
      assertEquals(HNil, emptyResult)

      val identityResult = in.collect(poly.identity)
      typed[HNil](identityResult)
      assertEquals(HNil, identityResult)

      val complexResult = in.collect(complex)
      typed[HNil](complexResult)
      assertEquals(HNil, complexResult)
    }

    { // non-HNil collect empty
      val in: Int :: String :: Double :: HNil = 1 :: "foo" :: 2.2 :: HNil

      val result = in.collect(empty)
      typed[HNil](result)
      assertEquals(HNil, result)
    }

    { // h collect identity
      val in: Int :: String :: Double :: HNil = 1 :: "foo" :: 2.2 :: HNil

      val result = in.collect(identity)
      typed[Int :: String :: Double :: HNil](result)
      assertEquals(in, result)
    }

    { // h collect complex
      val in: Int :: String :: Double :: HNil = 1 :: "foo" :: 2.2 :: HNil

      val result = in.collect(complex)
      typed[Double :: Int :: HNil](result)
      assertEquals(1.0 :: 1 :: HNil, result)
    }
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
    assertEquals(HNil, (HNil: HNil).mapCons('a'))

    assertEquals((('a' :: HNil) :: HNil), (HNil :: HNil).mapCons('a'))

    assertEquals(
      ('a' :: 1 :: HNil) :: ('a' :: "foo" :: HNil) :: ('a' :: 2.0 :: HNil) :: HNil,
      ((1 :: HNil) :: ("foo" :: HNil) :: (2.0 :: HNil) :: HNil).mapCons('a')
    )
  }

  @Test
  def testInterleave {
    assertEquals(('i' :: HNil) :: HNil, Interleave[Char, HNil].apply('i', HNil))

    assertEquals(('i' :: 1 :: HNil) :: (1 :: 'i' :: HNil) :: HNil,
      Interleave[Char, Int :: HNil].apply('i', 1 :: HNil)
    )

    assertEquals(
      ('i' :: 1 :: "foo" :: HNil) ::
      (1 :: 'i' :: "foo" :: HNil) ::
      (1 :: "foo" :: 'i' :: HNil) :: HNil,
      Interleave[Char, Int :: String :: HNil].apply('i', 1 :: "foo" :: HNil)
    )

    assertEquals(
      ('i' :: 1 :: "foo" :: 2.0 :: HNil) ::
      (1 :: 'i' :: "foo" :: 2.0 :: HNil) ::
      (1 :: "foo" :: 'i' :: 2.0 :: HNil) ::
      (1 :: "foo" :: 2.0 :: 'i' :: HNil) :: HNil,
      Interleave[Char, Int :: String :: Double :: HNil].apply('i', 1 :: "foo" :: 2.0 :: HNil)
    )
  }

  @Test
  def testFlatMapInterleave {
    assertEquals(HNil, FlatMapInterleave[Char, HNil].apply('i', HNil))

    assertEquals(('i' :: HNil) :: HNil, FlatMapInterleave[Char, HNil :: HNil].apply('i', HNil :: HNil))

    assertEquals(
      ('i' :: 1 :: HNil) ::
      (1 :: 'i' :: HNil) ::
      ('i' :: 2 :: HNil) ::
      (2 :: 'i' :: HNil) :: HNil,
      FlatMapInterleave[Char, (Int :: HNil) :: (Int :: HNil) :: HNil]
        .apply('i', (1 :: HNil) :: (2 :: HNil) :: HNil)
    )
  }

  @Test
  def testPermutations {
    assertEquals(HNil :: HNil, HNil.permutations)

    assertEquals((1 :: HNil) :: HNil, (1 :: HNil).permutations)

    assertEquals(
      (1 :: "foo" :: HNil) ::
      ("foo" :: 1 :: HNil) :: HNil,
      (1 :: "foo" :: HNil).permutations
    )

    assertEquals(
      (1 :: "foo" :: 2.0 :: HNil) ::
      ("foo" :: 1 :: 2.0 :: HNil) ::
      ("foo" :: 2.0 :: 1 :: HNil) ::
      (1 :: 2.0 :: "foo" :: HNil) ::
      (2.0 :: 1 :: "foo" :: HNil) ::
      (2.0 :: "foo" :: 1 :: HNil) :: HNil,
      (1 :: "foo" :: 2.0 :: HNil).permutations
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

    // rotateLeft[_0]
    assertTypedSame[HNil](HNil, in0.rotateLeft[_0])
    assertTypedSame[I :: HNil](in1, in1.rotateLeft[_0])
    assertTypedSame[I :: S :: HNil](in2, in2.rotateLeft[_0])
    assertTypedSame[I :: S :: D :: HNil](in3, in3.rotateLeft[_0])
    assertTypedSame[I :: S :: D :: C :: HNil](in4, in4.rotateLeft[_0])

    // rotateLeft[N % Size == 0]
    assertTypedSame[I :: HNil](in1, in1.rotateLeft[_1])
    assertTypedSame[I :: HNil](in1, in1.rotateLeft[_2])
    assertTypedSame[I :: S :: HNil](in2, in2.rotateLeft[_2])
    assertTypedSame[I :: S :: HNil](in2, in2.rotateLeft[_4])
    assertTypedSame[I :: S :: D :: HNil](in3, in3.rotateLeft[_3])
    assertTypedSame[I :: S :: D :: HNil](in3, in3.rotateLeft[_6])
    assertTypedSame[I :: S :: D :: C :: HNil](in4, in4.rotateLeft[_4])
    assertTypedSame[I :: S :: D :: C :: HNil](in4, in4.rotateLeft[_8])

    // other
    assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, in2.rotateLeft[_1])
    assertTypedEquals[S :: D :: I :: HNil]("foo" :: 2.0 :: 1 :: HNil, in3.rotateLeft[_1])
    assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, in4.rotateLeft[_1])
    assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, in4.rotateLeft[_2])
    assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, in4.rotateLeft[_3])
    assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, in4.rotateLeft[_5])
    assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, in4.rotateLeft[_6])
  }

  @Test
  def testRotateRight {
    val in0 = HNil
    val in1 = 1 :: HNil
    val in2 = 1 :: "foo" :: HNil
    val in3 = 1 :: "foo" :: 2.0 :: HNil
    val in4 = 1 :: "foo" :: 2.0 :: 'a' :: HNil
    type S = String; type I = Int; type D = Double; type C = Char

    // rotateRight[_0]
    assertTypedSame[HNil](HNil, in0.rotateRight[_0])
    assertTypedSame[I :: HNil](in1, in1.rotateRight[_0])
    assertTypedSame[I :: S :: HNil](in2, in2.rotateRight[_0])
    assertTypedSame[I :: S :: D :: HNil](in3, in3.rotateRight[_0])
    assertTypedSame[I :: S :: D :: C :: HNil](in4, in4.rotateRight[_0])

    // rotateRight[N % Size == 0]
    assertTypedSame[I :: HNil](in1, in1.rotateRight[_1])
    assertTypedSame[I :: HNil](in1, in1.rotateRight[_2])
    assertTypedSame[I :: S :: HNil](in2, in2.rotateRight[_2])
    assertTypedSame[I :: S :: HNil](in2, in2.rotateRight[_4])
    assertTypedSame[I :: S :: D :: HNil](in3, in3.rotateRight[_3])
    assertTypedSame[I :: S :: D :: HNil](in3, in3.rotateRight[_6])
    assertTypedSame[I :: S :: D :: C :: HNil](in4, in4.rotateRight[_4])
    assertTypedSame[I :: S :: D :: C :: HNil](in4, in4.rotateRight[_8])

    // others
    assertTypedEquals[S :: I :: HNil]("foo" :: 1 :: HNil, in2.rotateRight[_1])
    assertTypedEquals[D :: I :: S :: HNil](2.0 :: 1 :: "foo" :: HNil, in3.rotateRight[_1])
    assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, in4.rotateRight[_1])
    assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, in4.rotateRight[_2])
    assertTypedEquals[S :: D :: C :: I :: HNil]("foo" :: 2.0 :: 'a' :: 1 :: HNil, in4.rotateRight[_3])
    assertTypedEquals[C :: I :: S :: D :: HNil]('a' :: 1 :: "foo" :: 2.0 :: HNil, in4.rotateRight[_5])
    assertTypedEquals[D :: C :: I :: S :: HNil](2.0 :: 'a' :: 1 :: "foo" :: HNil, in4.rotateRight[_6])
  }
}
