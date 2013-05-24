/*
 * Copyright (c) 2011 Miles Sabin 
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

class SybClassTests {
  import org.junit.Test
  import org.junit.Assert._

  import SybClass._
  import TypeOperators._
  import HList._

  def typed[T](t : => T) {}
  
  object gsizeAll extends Poly1 {
    implicit def caseString = at[String](_.length)
    implicit def default[T](implicit data : Data[this.type, T, Int]) = at[T](1+data.gmapQ(_).sum)
  }

  object gsize extends Poly1 {
    implicit def caseInt = at[Int](_ => 1)
    implicit def caseString = at[String](_.length)
    implicit def default[T] = at[T](_ => 1)
  }

  def gsizeAll2[T](t : T)(implicit e : Everything[gsize.type, plus.type, T]) = everything(gsize)(plus)(t)

  object incAll extends Poly1 {
    implicit def caseInt = at[Int](_+1)
    implicit def caseString = at[String](_+"*")
    implicit def default[T](implicit data : DataT[this.type, T]) = at[T](data.gmapT)
  }

  object inc extends Poly1 {
    implicit def caseInt = at[Int](_+1)
    implicit def caseString = at[String](_+"*")
    implicit def default[T] = at[T](identity)
  }
  
  def incAll2[T](t : T)(implicit e : Everywhere[inc.type, T]) : T = everywhere(inc)(t)
  
  @Test
  def testGMapQ {
    val p = (23, "foo")
    val ps = gsizeAll(p)
    assertEquals(5, ps)

    val t = (23, "foo", true, 1.0)
    val ts = gsizeAll(t)
    assertEquals(7, ts)

    val l = List(1, 2, 3) 
    val ls = gsizeAll(l)
    assertEquals(4, ls)

    val lp = List(("foo", 23), ("bar", 24))
    val lps = gsizeAll(lp)
    assertEquals(11, lps)
  }

  @Test
  def testGMapT {
    val p = (23, "foo")
    val pi = incAll(p)
    assertEquals((24, "foo*"), pi)

    val t = (23, "foo", true, 1.0)
    val ti = incAll(t)
    assertEquals((24, "foo*", true, 1.0), ti)

    val o = Option(23)
    val oi = incAll(o)
    assertEquals(Some(24), oi)

    val e : Either[String, Int] = Right(23)
    val ei = incAll(e)
    assertEquals(Right(24), ei)

    val lo = List(Some(1), None, Some(2))
    val loi = incAll(lo)
    assertEquals(List(Some(2), None, Some(3)), loi)
  }

  @Test
  def testEverything {
    val e1 = everything(gsize)(plus)(23)
    typed[Int](e1)
    assertEquals(1, e1)
    
    val e2 = everything(gsize)(plus)("foo")
    typed[Int](e2)
    assertEquals(3, e2)
    
    val e3 = everything(gsize)(plus)((23, "foo"))
    typed[Int](e3)
    assertEquals(5, e3)

    val e4 = everything(gsize)(plus)(List(1, 2, 3, 4))
    typed[Int](e4)
    assertEquals(5, e4)
    
    val is = gsizeAll2(23)
    typed[Int](is)
    assertEquals(1, is)

    val ss = gsizeAll2("foo")
    typed[Int](ss)
    assertEquals(3, ss)

    val ps = gsizeAll2(23, "foo")
    typed[Int](ps)
    assertEquals(5, ps)

    val ls = gsizeAll2(List(1, 2, 3, 4))
    typed[Int](ls)
    assertEquals(5, ls)

    val lps = gsizeAll2(List(("foo", 23), ("bar", 24)))
    typed[Int](lps)
    assertEquals(11, lps)
  }

  @Test
  def testEverywhere {
    val pi = incAll2((23, "foo"))
    typed[(Int, String)](pi)
    assertEquals((24, "foo*"), pi)

    val oi = incAll2(Option(23))
    typed[Option[Int]](oi)
    assertEquals(Some(24), oi)

    val ei = incAll2(Right(23) : Either[String, Int])
    typed[Either[String, Int]](ei)
    assertEquals(Right(24), ei)

    val loi = incAll2(List(Some(1), None, Some(2)))
    typed[List[Option[Int]]](loi)
    assertEquals(List(Some(2), None, Some(3)), loi)

    val e1 = everywhere(inc)(23)
    typed[Int](e1)
    assertEquals(24, e1)

    val e2 = everywhere(inc)(Option(23))
    typed[Option[Int]](e2)
    assertEquals(Option(24), e2)
      
    val e3 = everywhere(inc)(List(23))
    typed[List[Int]](e3)
    assertEquals(List(24), e3)
        
    val e4 = everywhere(inc)(List(Option(23)))
    typed[List[Option[Int]]](e4)
    assertEquals(List(Option(24)), e4)
          
    val e5 = everywhere(inc)(Option(List(23)))
    typed[Option[List[Int]]](e5)
    assertEquals(Option(List(24)), e5)

    val e6 = everywhere(inc)(List(List(List(23))))
    typed[List[List[List[Int]]]](e6)
    assertEquals(List(List(List(24))), e6)

    val e7 = everywhere(inc)(Option(Option(Option(23))))
    typed[Option[Option[Option[Int]]]](e7)
    assertEquals(Option(Option(Option(24))), e7)

    val e8 = everywhere(inc)(List(Option(List(Option(List(Option(23)))))))
    typed[List[Option[List[Option[List[Option[Int]]]]]]](e8)
    assertEquals(List(Option(List(Option(List(Option(24)))))), e8)
  }
  
  @Test
  def testHList {
    val l = 23 :: "foo" :: true :: 2.0 :: HNil
    
    val li = everywhere(inc)(l)
    typed[Int :: String :: Boolean :: Double :: HNil](li)
    assertEquals(24 :: "foo*" :: true :: 2.0 :: HNil, li)
    
    val ls = everything(gsize)(plus)(l)
    typed[Int](ls)
    assertEquals(7, ls)
    
    val l2 = 23 :: ("foo" :: true :: HNil) :: 2.0 :: HNil 

    val li2 = everywhere(inc)(l2)
    typed[Int :: (String :: Boolean :: HNil) :: Double :: HNil](li2)
    assertEquals(24 :: ("foo*" :: true :: HNil) :: 2.0 :: HNil, li2)
    
    val ls2 = everything(gsize)(plus)(l2)
    typed[Int](ls2)
    assertEquals(8, ls2)
  }

  case class A(x: Int, y: Boolean, z: Int)
  
  object flip extends Poly1 {
    implicit def apply[T] = at[T](identity)
    implicit def caseBoolean = at[Boolean](!_)
  }

  @Test
  def testHListIso {
    // Fails if moved after testHListIso2 
    val input =    1 :: A(1, true,  2) :: HNil
    val expected = 1 :: A(1, false, 2) :: HNil
    
    val result = everywhere(flip)(input)
    
    assertEquals(expected, result) 
  }
  
  case class Address(street : String, city : String, postcode : String)
  case class Person(name : String, age : Int, address : Address)
  
  @Test
  def testHListIso2 {
    
    val p1 = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
    
    val p2 = everywhere(inc)(p1)
    assertEquals(Person("Joe Grey*", 38, Address("Southover Street*", "Brighton*", "BN2 9UA*")), p2)

    val s1 = everything(gsize)(plus)(p1)
    assertEquals(42, s1)
  }
}
