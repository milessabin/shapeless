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

class HMapTests {

  class BiMapIS[K, V]
  implicit val intToString = new BiMapIS[Int, String]
  implicit val stringToInt = new BiMapIS[String, Int]
  
  @Test
  def testBasics: Unit = {
    val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13)
    
    illTyped("""
      val hm2 = HMap[BiMapIS](23 -> "foo", 23 -> 13)
    """)
    
    val s1 = hm.get(23)
    assertTrue(isDefined(s1))
    typed[Option[String]](s1)
    assertEquals(Some("foo"), s1)

    val i1 = hm.get("bar")
    assertTrue(isDefined(i1))
    typed[Option[Int]](i1)
    assertEquals(Some(13), i1)
  }
  
  @Test
  def testPoly: Unit = {
    val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13)
    import hm._
    
    // Map over an HList
    val l1 = 23 :: "bar" :: 23 :: "bar" :: HNil
    val l2 = l1 map hm
    typed[String :: Int :: String :: Int :: HNil](l2)
    assertEquals("foo" :: 13 :: "foo" :: 13 :: HNil, l2)
    
    // Use as an argument to a HoF
    def pairApply(f: Poly1)(implicit ci : f.Case[Int], cs : f.Case[String]) = (f(23), f("bar"))

    val a1 = pairApply(hm)
    typed[(String, Int)](a1)
    assertEquals(("foo", 13), a1)
  }
  
  @Test
  def testNatTrans: Unit = {
    val nt = HMap[(Set ~?> Option)#λ](Set("foo") -> Option("bar"), Set(23) -> Option(13))
    
    illTyped("""
      val nt2 = HMap[(Set ~?> Option)#λ](Set("foo") -> Option(13), Set(23) -> Option(13))
    """)
    illTyped("""
      val nt3 = HMap[(Set ~?> Option)#λ](Set("foo") -> Option("bar"), "foo" -> 23)
    """)

    // Needed to allow V to be inferred in get
    implicit object SO extends (Set ~?> Option)
    
    val o1 = nt.get(Set("foo"))
    assertTrue(isDefined(o1))
    typed[Option[Option[String]]](o1)
    assertEquals(Some(Some("bar")), o1)
    
    val o2 = nt.get(Set(23))
    assertTrue(isDefined(o2))
    typed[Option[Option[Int]]](o2)
    assertEquals(Some(Some(13)), o2)
  }
  
  @Test
  def testPolyNatTrans: Unit = {
    val nt = HMap[(Set ~?> Option)#λ](Set("foo") -> Option("bar"), Set(23) -> Option(13))
    import nt._
    
    // Needed to allow V to be inferred in Case1 resolution (ie. map and pairApply)
    implicit object SO extends (Set ~?> Option)
    
    // Map over an HList
    val l1 = Set("foo") :: Set(23) :: HNil
    val l2 = l1 map nt
    typed[Option[String] :: Option[Int] :: HNil](l2)
    assertEquals(Some("bar") :: Some(13) :: HNil, l2)

    // Use as an argument to a HoF
    def pairApply(f: Poly1)(implicit cs : f.Case[Set[String]], ci : f.Case[Set[Int]]) = (f(Set("foo")), f(Set(23)))
    
    val a1 = pairApply(nt)
    typed[(Option[String], Option[Int])](a1)
    assertEquals((Option("bar"), Option(13)), a1)
  }
  
  @Test
  def testIdKeyNatTrans: Unit = {
    val nt = HMap[(Id ~?> Option)#λ]("foo" -> Option("bar"), 23 -> Option(13))
    
    illTyped("""
      val nt2 = HMap[(Id ~?> Option)#λ]("foo" -> Option(13), 23 -> Option(13))
    """)
    illTyped("""
      val nt3 = HMap[(Id ~?> Option)#λ]("foo" -> Option("bar"), Set(23) -> Option(13))
    """)
    
    // Needed to allow V to be inferred in get
    implicit object IO extends (Id ~?> Option)
    
    val o1 = nt.get("foo")
    assertTrue(isDefined(o1))
    typed[Option[Option[String]]](o1)
    assertEquals(Some(Some("bar")), o1)
    
    val o2 = nt.get(23)
    assertTrue(isDefined(o2))
    typed[Option[Option[Int]]](o2)
    assertEquals(Some(Some(13)), o2)
  }  
  
  @Test
  def testIdValueNatTrans: Unit = {
    val nt = HMap[(Option ~?> Id)#λ](Option("foo") -> "bar", Option(23) -> 13)
    
    illTyped("""
      val nt2 = HMap[(Option ~?> Id)#λ](Option("foo") -> 13, Option(23) -> 13)
    """)
    illTyped("""
      val nt3 = HMap[(Option ~?> Id)#λ](Option("foo") -> "bar", Option(23) -> Set(13))
    """)

    
    // Needed to allow V to be inferred in get
    implicit object IO extends (Id ~?> Option)
    
    val o1 = nt.get(Option("foo"))
    assertTrue(isDefined(o1))
    typed[Option[String]](o1)
    assertEquals(Some("bar"), o1)
    
    val o2 = nt.get(Option(23))
    assertTrue(isDefined(o2))
    typed[Option[Int]](o2)
    assertEquals(Some(13), o2)
  }

  trait M[A]

  class Mapping[K, V]
  implicit def mappingFromM[A, B <: M[A]] = new Mapping[A,B]

  case class X(x: Int) extends M[X.type]
  object X

  case class Y(x: String) extends M[Y.type]
  object Y

  @Test
  def testSingleton: Unit = {

    val hm = HMap[Mapping](X -> X(13), Y -> Y("foo"))

    illTyped("""
      val hm2 = HMap[Mapping](X -> Y("foo"), Y -> X(13))
    """)

    val x = hm.get(X)
    assertTrue(isDefined(x))
    typed[Option[X]](x)
    assertEquals(Some(X(13)), x)

    val y = hm.get(Y)
    assertTrue(isDefined(y))
    typed[Option[Y]](y)
    assertEquals(Some(Y("foo")), y)
  }
}
