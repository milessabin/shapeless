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

import shapeless.test._
import testutil._

class SizedTests {
  import nat._
  import syntax.sized._
  
  @Test
  def testBasics: Unit = {
    val l0 = List.empty[Int]
    val l1 = List(1)
    val l2 = List(1, 2)
    val l3 = List(1, 2, 3)
    
    val nl0 = l0.sized(0)
    assertTrue(nl0.isDefined)
    val nl0b = l0.sized(1)
    assertTrue(nl0b.isEmpty)
    val nl0c = l0.sized(2)
    assertTrue(nl0c.isEmpty)
    val nl0d = l0.sized(3)
    assertTrue(nl0d.isEmpty)
    
    illTyped("""
    val h0 = nl0.get.head
    """)
    illTyped("""
    val t0 = nl0.get.tail
    """)
    
    val nl1 = l1.sized(0)
    assertTrue(nl1.isEmpty)
    val nl1b = l1.sized(1)
    assertTrue(nl1b.isDefined)
    val nl1c = l1.sized(2)
    assertTrue(nl1c.isEmpty)
    val nl1d = l1.sized(3)
    assertTrue(nl1d.isEmpty)

    val h1 = nl1b.get.head
    val t1 = nl1b.get.tail

    illTyped("""
    val t1b = nl1b.get.tail.tail
    """)

    val nl2 = l2.sized(0)
    assertTrue(nl2.isEmpty)
    val nl2b = l2.sized(1)
    assertTrue(nl2b.isEmpty)
    val nl2c = l2.sized(2)
    assertTrue(nl2c.isDefined)
    val nl2d = l2.sized(3)
    assertTrue(nl2d.isEmpty)
    
    val h2 = nl2c.get.head
    val t2 = nl2c.get.tail
    val t2b = nl2c.get.tail.tail

    illTyped("""
    val t2c = nl1c.get.tail.tail.tail
    """)

    val nl3 = l3.sized(0)
    assertTrue(nl3.isEmpty)
    val nl3b = l3.sized(1)
    assertTrue(nl3b.isEmpty)
    val nl3c = l3.sized(2)
    assertTrue(nl3c.isEmpty)
    val nl3d = l3.sized(3)
    assertTrue(nl3d.isDefined)

    val h3 = nl3d.get.head
    val t3 = nl3d.get.tail
    val t3b = nl3d.get.tail.tail
    val t3c = nl3d.get.tail.tail.tail

    illTyped("""
    val t3d = nl1d.get.tail.tail.tail.tail
    """)
    
    val rs = "foo".sized(3).get.unsized
    
    val rl = List(1, 2, 3).sized(3).get.unsized
    
    val s1 = "foo".sized(3)
    assertTrue(s1.isDefined)
    val s2 = "bar".sized(3)
    assertTrue(s2.isDefined)

    val s3 = s1.get ++ s2.get
    typed[Sized[String, _6]](s3)
    assertEquals("foobar", s3.unsized)
    
    val cs = for(x <- s1 ; y <- s2) yield x ++ y
    assertTrue(cs.isDefined)
    typed[Sized[String, _6]](cs.get)
    assertEquals("foobar", cs.get.unsized)
    
    val s4 = 'c' +: s1.get
    
    val ll1 = List(1, 2, 3).sized(3)
    assertTrue(ll1.isDefined)
    val ll2 = List(4, 5, 6).sized(3)
    assertTrue(ll2.isDefined)

    val ll3 = ll1.get ++ ll2.get
    typed[Sized[List[Int], _6]](ll3)
    assertEquals(List(1, 2, 3, 4, 5, 6), ll3.unsized)
    
    val cl = for(x <- ll1 ; y <- ll2) yield x ++ y
    assertTrue(cl.isDefined)
    typed[Sized[List[Int], _6]](cl.get)
    assertEquals(List(1, 2, 3, 4, 5, 6), cl.get.unsized)

    val s = cl.get.size
    val evens = cl.get.filter(_ % 2 == 0)
    
    val p = cl.get match {
      case Sized(a, b, _*) => {
        typed[Int](a)
        typed[Int](b)
        (a-b, a+b)
      }
      case _ => (9, 10)
    }
    typed[(Int, Int)](p)
    assertEquals((-1, 3), p)
    
    val j1 = ll1.get.take(1)
    
    val tk1 = cl.get.take(1)
    val tk4 = cl.get.take(4)

    illTyped("""
    val tk7 = cl.get.take(7)
    """)
    
    val dr1 = cl.get.drop(1)
    val dr4 = cl.get.drop(4)

    illTyped("""
    val dr7 = cl.get.drop(7)
    """)
    
    val (pr1, sf1) = cl.get.splitAt(1)
    val (pr4, sf4) = cl.get.splitAt(4)

    illTyped("""
    val (pr7, sf7) = cl.get.splitAt(7)
    """)
    
    val ml = cl.get map (_.toString)
    typed[Sized[List[String], _6]](ml)
    assertEquals(List("1", "2", "3", "4", "5", "6"), ml.unsized)

    import scala.collection.immutable.IndexedSeq

    val is0 = Sized()
    typed[Sized[IndexedSeq[Nothing], _0]](is0)
    val is1 = Sized("foo")
    typed[Sized[IndexedSeq[String], _1]](is1)
    val is2 = Sized("foo", "bar")
    typed[Sized[IndexedSeq[String], _2]](is2)
    
    val isl0 = Sized[List]()
    typed[Sized[List[Nothing], _0]](isl0)
    val isl1 = Sized[List]("foo")
    typed[Sized[List[String], _1]](isl1)
    val isl2 = Sized[List]("foo", "bar")
    typed[Sized[List[String], _2]](isl2)

    // Checking that Sized constructor is private
    illTyped(""" new Sized[List[String], _2](List("1", "2")) """)

    val isa0 = Sized[Array]()
    typed[Sized[Array[Nothing], _0]](isa0)
    val isa1 = Sized[Array]("foo")
    typed[Sized[Array[String], _1]](isa1)
    val isa2 = Sized[Array]("foo", "bar")
    typed[Sized[Array[String], _2]](isa2)

    val set = Set(1, 2)
    illTyped(""" set.sized(2) """)
    illTyped(""" Sized[Set](1, 1, 1) """)

    illTyped(""" new Sized[Set[Int], _3](Set(1, 1, 1)) """)
  }

  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit

  val a : Apple = Apple()
  val p : Pear = Pear()
  val b : Banana = Banana()

  val apap = a :: p :: a :: p :: HNil
  val apapList = a :: p :: a :: p :: Nil
  val apapSized = Sized[List](a, p, a, p)
  
  val apbp = a :: p :: b :: p :: HNil
  val apbpList = a :: p :: b :: p :: Nil
  val apbpSized = Sized[List](a, p, b, p)


  val si = Set.empty[Int]
  val ss = Set.empty[String]
  val sd = Set.empty[Double]
  
  val sisssisisd = si :: ss :: si :: si :: sd :: HNil
  val sisssisisdList = si :: ss :: si :: si :: sd :: Nil
  val sisssisisdSized = Sized[List](si, ss, si, si, sd)

  
  trait Ctv[-T]

  val ci: Ctv[Int] = new Ctv[Int] {}
  val cs: Ctv[String] = new Ctv[String] {}
  val cd: Ctv[Double] = new Ctv[Double] {}
  
  val cicscicicd = ci :: cs :: ci :: ci :: cd :: HNil
  val cicscicicdList = ci :: cs :: ci :: ci :: cd :: Nil
  val cicscicicdSized = Sized[List](ci, cs, ci, ci, cd)

  
  trait M[T]

  val mi: M[Int] = new M[Int] {}
  val ms: M[String] = new M[String] {}
  val md: M[Double] = new M[Double] {}
  
  val mimsmimimd = mi :: ms :: mi :: mi :: md :: HNil
  val mimsmimimdList = mi :: ms :: mi :: mi :: md :: Nil
  val mimsmimimdSized = Sized[List](mi, ms, mi, mi, md)

  import language.existentials
  val mExist: M[_] = new M[Double] {}
  val mimsmimemd = mi :: ms :: mi :: mExist :: md :: HNil
  val mimsmimemdList = mi :: ms :: mi :: mExist :: md :: Nil
  val mimsmimemdSized = Sized[List](mi, ms, mi, mExist, md)

  
  trait M2[A,B]

  val m2i: M2[Int, Unit] = new M2[Int, Unit] {}
  val m2s: M2[String, Unit] = new M2[String, Unit] {}
  val m2d: M2[Double, Unit] = new M2[Double, Unit] {}
  
  val m2im2sm2im2im2d = m2i :: m2s :: m2i :: m2i :: m2d :: HNil
  val m2im2sm2im2im2dList = m2i :: m2s :: m2i :: m2i :: m2d :: Nil
  val m2im2sm2im2im2dSized = Sized[List](m2i, m2s, m2i, m2i, m2d)

  val m2iExist: M2[Int, _] = new M2[Int, Unit] {}
  val m2sExist: M2[String, _] = new M2[String, Unit] {}
  val m2dExist: M2[Double, _] = new M2[Double, Unit] {}
  
  val m2eim2esm2eim2eem2ed = m2iExist :: m2sExist :: m2iExist :: m2iExist :: m2dExist :: HNil
  val m2eim2esm2eim2eem2edList = m2iExist :: m2sExist :: m2iExist :: m2iExist :: m2dExist :: Nil
  val m2eim2esm2eim2eem2edSized = Sized[List](m2iExist, m2sExist, m2iExist, m2iExist, m2dExist)


  @Test
  def testToHList: Unit = {
    def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B): Unit = {}

    val hlApap = apapSized.toHList
    equalInferredTypes(Nat._4, hlApap.length)
    equalInferredTypes(apapList.head, hlApap(Nat._0))
    equalInferredTypes(apapList.head, hlApap(Nat._1))
    equalInferredTypes(apapList.head, hlApap(Nat._2))
    equalInferredTypes(apapList.head, hlApap(Nat._3))
    assertEquals(apap, hlApap)

    val hlApbp = apbpSized.toHList
    equalInferredTypes(Nat._4, hlApbp.length)
    equalInferredTypes(apbpList.head, hlApbp(Nat._0))
    equalInferredTypes(apbpList.head, hlApbp(Nat._1))
    equalInferredTypes(apbpList.head, hlApbp(Nat._2))
    equalInferredTypes(apbpList.head, hlApbp(Nat._3))
    assertEquals(apbp, hlApbp)

    val hlCicscicicd = cicscicicdSized.toHList
    equalInferredTypes(Nat._5, hlCicscicicd.length)
    equalInferredTypes(cicscicicdList.head, hlCicscicicd(Nat._0))
    equalInferredTypes(cicscicicdList.head, hlCicscicicd(Nat._1))
    equalInferredTypes(cicscicicdList.head, hlCicscicicd(Nat._2))
    equalInferredTypes(cicscicicdList.head, hlCicscicicd(Nat._3))
    equalInferredTypes(cicscicicdList.head, hlCicscicicd(Nat._4))
    assertEquals(cicscicicd, hlCicscicicd)
    
    val hlMimsmimimd = mimsmimimdSized.toHList
    equalInferredTypes(Nat._5, hlMimsmimimd.length)
    // equalInferredTypes(mimsmimimdList.head, hlMimsmimimd(Nat._0))
    // equalInferredTypes(mimsmimimdList.head, hlMimsmimimd(Nat._1))
    // equalInferredTypes(mimsmimimdList.head, hlMimsmimimd(Nat._2))
    // equalInferredTypes(mimsmimimdList.head, hlMimsmimimd(Nat._3))
    // equalInferredTypes(mimsmimimdList.head, hlMimsmimimd(Nat._4))
    typed[M[_ >: Int with Double with String]](hlMimsmimimd(Nat._0))
    typed[M[_ >: Int with Double with String]](hlMimsmimimd(Nat._1))
    typed[M[_ >: Int with Double with String]](hlMimsmimimd(Nat._2))
    typed[M[_ >: Int with Double with String]](hlMimsmimimd(Nat._3))
    typed[M[_ >: Int with Double with String]](hlMimsmimimd(Nat._4))
    assertEquals(mimsmimimd, hlMimsmimimd)
    
    val hlMimsmimemd = mimsmimemdSized.toHList
    equalInferredTypes(Nat._5, hlMimsmimemd.length)
    // equalInferredTypes(mimsmimemdList.head, hlMimsmimemd(Nat._0))
    // equalInferredTypes(mimsmimemdList.head, hlMimsmimemd(Nat._1))
    // equalInferredTypes(mimsmimemdList.head, hlMimsmimemd(Nat._2))
    // equalInferredTypes(mimsmimemdList.head, hlMimsmimemd(Nat._3))
    // equalInferredTypes(mimsmimemdList.head, hlMimsmimemd(Nat._4))
    typed[M[_]](hlMimsmimemd(Nat._0))
    typed[M[_]](hlMimsmimemd(Nat._1))
    typed[M[_]](hlMimsmimemd(Nat._2))
    typed[M[_]](hlMimsmimemd(Nat._3))
    typed[M[_]](hlMimsmimemd(Nat._4))
    assertEquals(mimsmimemd, hlMimsmimemd)
    
    val hlM2im2sm2im2im2d = m2im2sm2im2im2dSized.toHList
    equalInferredTypes(Nat._5, hlM2im2sm2im2im2d.length)
    // equalInferredTypes(m2im2sm2im2im2dList.head, hlM2im2sm2im2im2d(Nat._0))
    // equalInferredTypes(m2im2sm2im2im2dList.head, hlM2im2sm2im2im2d(Nat._1))
    // equalInferredTypes(m2im2sm2im2im2dList.head, hlM2im2sm2im2im2d(Nat._2))
    // equalInferredTypes(m2im2sm2im2im2dList.head, hlM2im2sm2im2im2d(Nat._3))
    // equalInferredTypes(m2im2sm2im2im2dList.head, hlM2im2sm2im2im2d(Nat._4))
    typed[M2[_ >: Double with Int with String, Unit]](hlM2im2sm2im2im2d(Nat._0))
    typed[M2[_ >: Double with Int with String, Unit]](hlM2im2sm2im2im2d(Nat._1))
    typed[M2[_ >: Double with Int with String, Unit]](hlM2im2sm2im2im2d(Nat._2))
    typed[M2[_ >: Double with Int with String, Unit]](hlM2im2sm2im2im2d(Nat._3))
    typed[M2[_ >: Double with Int with String, Unit]](hlM2im2sm2im2im2d(Nat._4))
    assertEquals(m2im2sm2im2im2d, hlM2im2sm2im2im2d)
    
    val hlM2eim2esm2eim2eem2ed = m2eim2esm2eim2eem2edSized.toHList
    equalInferredTypes(Nat._5, hlM2eim2esm2eim2eem2ed.length)
    // equalInferredTypes(m2eim2esm2eim2eem2edList.head, hlM2eim2esm2eim2eem2ed(Nat._0))
    // equalInferredTypes(m2eim2esm2eim2eem2edList.head, hlM2eim2esm2eim2eem2ed(Nat._1))
    // equalInferredTypes(m2eim2esm2eim2eem2edList.head, hlM2eim2esm2eim2eem2ed(Nat._2))
    // equalInferredTypes(m2eim2esm2eim2eem2edList.head, hlM2eim2esm2eim2eem2ed(Nat._3))
    // equalInferredTypes(m2eim2esm2eim2eem2edList.head, hlM2eim2esm2eim2eem2ed(Nat._4))
    typed[M2[_ >: Double with Int with String, _]](hlM2eim2esm2eim2eem2ed(Nat._0))
    typed[M2[_ >: Double with Int with String, _]](hlM2eim2esm2eim2eem2ed(Nat._1))
    typed[M2[_ >: Double with Int with String, _]](hlM2eim2esm2eim2eem2ed(Nat._2))
    typed[M2[_ >: Double with Int with String, _]](hlM2eim2esm2eim2eem2ed(Nat._3))
    typed[M2[_ >: Double with Int with String, _]](hlM2eim2esm2eim2eem2ed(Nat._4))
    assertEquals(m2eim2esm2eim2eem2ed, hlM2eim2esm2eim2eem2ed)
  }

  @Test
  def testAt: Unit = {
    val ss = Sized[List](0, 1, 2)
    typed[Int](ss(0))
    typed[Int](ss(1))
    typed[Int](ss(2))

    assertEquals(ss[_0], 0)
    assertEquals(ss(0), 0)
    assertEquals(ss.at[_0], 0)
    assertEquals(ss.at(0), 0)

    assertEquals(ss[_1], 1)
    assertEquals(ss(1), 1)
    assertEquals(ss.at[_1], 1)
    assertEquals(ss.at(1), 1)

    assertEquals(ss[_2], 2)
    assertEquals(ss(2), 2)
    assertEquals(ss.at[_2], 2)
    assertEquals(ss.at(2), 2)

    illTyped("""
      ss(-1)
    """)

    illTyped("""
      ss(3)
    """)
  }

  @Test
  def testTupled: Unit = {
    val ss = Sized[List](0, 1, 2)
    val r1 = ss.tupled
    assertTypedEquals[(Int, Int, Int)]((0, 1, 2), r1)

    val (x, y, z) = ss.tupled
    assertTypedEquals[Int](0, x)
    assertTypedEquals[Int](1, y)
    assertTypedEquals[Int](2, z)
  }

  @Test
  def testEquals: Unit = {
    val s1 = Sized[List](1, 2, 3)
    val s2 = Sized[List](1, 2, 3)
    val s3 = Sized[List](1, 2, 4)
    assert(s1 == s2)
    assertFalse(s1 == s3)
    assertFalse(s1 == List(1, 2, 3))
  }

  @Test
  def testHashCode: Unit = {
    val s1 = Sized[List](1, 2, 3)
    val s2 = Sized[List](1, 2, 3)

    assertEquals(s1.hashCode, List(1, 2, 3).hashCode)
    assertEquals(s1.hashCode, s2.hashCode)
  }
}
