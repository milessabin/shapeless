/*
 * Copyright (c) 2011-13 Miles Sabin 
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
    implicit def default[T](implicit data : DataT[this.type, T, T]) = at[T](data.gmapT)
  }

  object inc extends Poly1 {
    implicit def caseInt = at[Int](_+1)
    implicit def caseString = at[String](_+"*")
  }
  
  def incAll2[T](t : T)(implicit e : Everywhere[inc.type, T]) = everywhere(inc)(t)

  sealed trait Fruit
  case class Apple(i: Int) extends Fruit
  case class Pear(i: Int) extends Fruit
  case class Banana(i: Int) extends Fruit
  case class Orange(i: Int) extends Fruit

  object showFruit extends Poly1 {
    implicit def caseApple  = at[Apple] (_ => "Pomme")
    implicit def casePear   = at[Pear]  (_ => "Poire")
    implicit def caseBanana = at[Banana](_ => "Banane")
    implicit def caseOrange = at[Orange](_ => "Orange")
  }
  
  object cycleFruit extends Poly1 {
    implicit def caseApple  = at[Apple] { case Apple(i)  => Pear(i) }
    implicit def casePear   = at[Pear]  { case Pear(i)   => Banana(i) }
    implicit def caseBanana = at[Banana]{ case Banana(i) => Orange(i) }
    implicit def caseOrange = at[Orange]{ case Orange(i) => Apple(i) }
  }
  
  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

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
  def testAtoms {
    val result1 = everywhere(inc)(23)
    typed[Int](result1)
    assertEquals(24, result1)
    
    val result2 = everywhere(inc)("foo")
    typed[String](result2)
    assertEquals("foo*", result2)
    
    val result3 = everywhere(showFruit)(Apple(1))
    typed[String](result3)
    assertEquals("Pomme", result3)

    val result4 = everywhere(inc)(Apple(1))
    typed[Apple](result4)
    assertEquals(Apple(2), result4)

    val result5 = everywhere(cycleFruit)(Apple(1))
    typed[Pear](result5)
    assertEquals(Pear(1), result5)

//    val result6 = everywhere(showFruit)(Apple(1) : Fruit)
//    typed[String](result6)
//    assertEquals("Pomme", result6)
    
    val result7 = everywhere(inc)(Apple(1) : Fruit)
    typed[Fruit](result7)
    assertEquals(Apple(2), result7)

//    val result8 = everywhere(cycleFruit)(Apple(1) : Fruit)
//    typed[Fruit](result8)
//    assertEquals(Pear(1), result8)
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
    
    val l3 = 23 :: 2.0 :: ("foo" :: true :: HNil) :: HNil

    val li3 = everywhere(inc)(l3)
    typed[Int :: Double :: (String :: Boolean :: HNil) :: HNil](li3)
    assertEquals(24 :: 2.0 :: ("foo*" :: true :: HNil) :: HNil, li3)
    
    val ls3 = everything(gsize)(plus)(l3)
    typed[Int](ls3)
    assertEquals(8, ls3)
  }

  @Test
  def testCoproduct {
    type ISBT = Int :+: String :+: Boolean :+: (String, String):+: CNil
    
    val ci = Coproduct[ISBT](23)
    val cs = Coproduct[ISBT]("foo")
    val cb = Coproduct[ISBT](true)
    val ct = Coproduct[ISBT]("bar", "baz")
    
    val cii = everywhere(inc)(ci)
    typed[ISBT](cii)
    assertEquals(Inl(24), cii)
    
    val csi = everywhere(inc)(cs)
    typed[ISBT](csi)
    assertEquals(Inr(Inl("foo*")), csi)
    
    val cbi = everywhere(inc)(cb)
    typed[ISBT](cbi)
    assertEquals(Inr(Inr(Inl(true))), cbi)
    
    val cti = everywhere(inc)(ct)
    typed[ISBT](cti)
    assertEquals(Inr(Inr(Inr(Inl(("bar*", "baz*"))))), cti)
    
    val cis = everything(gsize)(plus)(ci)
    typed[Int](cis)
    assertEquals(2, cis)
    
    val css = everything(gsize)(plus)(cs)
    typed[Int](css)
    assertEquals(4, css)
    
    val cbs = everything(gsize)(plus)(cb)
    typed[Int](cbs)
    assertEquals(2, cbs)
    
    val cts = everything(gsize)(plus)(ct)
    typed[Int](cts)
    assertEquals(8, cts)
  }

  case class A(x: Int, y: Boolean, z: Int)
  
  object flip extends Poly1 {
    implicit def apply[T] = at[T](identity)
    implicit def caseBoolean = at[Boolean](!_)
  }

  @Test
  def testGeneric1 {
    val input = A(1, true,  2)
    val expected = A(1, false, 2)
    
    val result = everywhere(flip)(input)
    
    assertEquals(expected, result) 
  }
  
  @Test
  def testGeneric2 {
    val input = List(A(1, true,  2))
    val expected = List(A(1, false, 2))
    
    val result = everywhere(flip)(input)
    
    assertEquals(expected, result) 
  }

  @Test
  def testGeneric3 {
    val input =    1 :: A(1, true,  2) :: HNil
    val expected = 1 :: A(1, false, 2) :: HNil
    
    val result = everywhere(flip)(input)
    
    assertEquals(expected, result)
  }

  @Test
  def testGeneric4 {
    val input =    (1, A(1, true,  2))
    val expected = (1, A(1, false, 2))
    
    val result = everywhere(flip)(input)
    
    assertEquals(expected, result) 
  }

  case class Address(street : String, city : String, postcode : String)
  case class Person(name : String, age : Int, address : Address)

  @Test
  def testGeneric5 {
    val input = Address("Southover Street", "Brighton", "BN2 9UA")
    val expected = Address("Southover Street*", "Brighton*", "BN2 9UA*")
    
    val result = everywhere(inc)(input)
    assertEquals(expected, result)

    val result2 = everything(gsize)(plus)(input)
    assertEquals(32, result2)
  }

  @Test
  def testGeneric6 {
    val input = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
    val expected = Person("Joe Grey*", 38, Address("Southover Street*", "Brighton*", "BN2 9UA*"))
    
    val result = everywhere(inc)(input)
    assertEquals(expected, result)

    val result2 = everything(gsize)(plus)(input)
    assertEquals(42, result2)
  }

  @Test
  def testHList2 {
    val input = Apple(1) :: Pear(2) :: Banana(3) :: Orange(4) :: HNil
    val expected = Pear(1) :: Banana(2) :: Orange(3) :: Apple(4) :: HNil
    
    val result = everywhere(cycleFruit)(input)
    typed[Pear :: Banana :: Orange :: Apple :: HNil](result)
    assertEquals(expected, result)
  }
  
  @Test
  def testHList3 {
    val input = Apple(1) :: Pear(2) :: Banana(3) :: Orange(4) :: HNil
    val expected = "Pomme" :: "Poire" :: "Banane" :: "Orange" :: HNil
    
    val result = everywhere(showFruit)(input)
    typed[String :: String :: String :: String :: HNil](result)
    assertEquals(expected, result)
  }
  
  @Test
  def testCoproduct2 {
    type APBO = Apple :+: Pear :+: Banana :+: Orange :+: CNil
    type PBOA = Pear :+: Banana :+: Orange :+: Apple :+: CNil
    
    val input1 = Coproduct[APBO](Apple(1))
    val expected1 = Coproduct[PBOA](Pear(1)) 
    
    val result1 = everywhere(cycleFruit)(input1)
    typed[PBOA](result1)
    assertEquals(expected1, result1)

    val input2 = Coproduct[APBO](Pear(2))
    val expected2 = Coproduct[PBOA](Banana(2)) 
    
    val result2 = everywhere(cycleFruit)(input2)
    typed[PBOA](result2)
    assertEquals(expected2, result2)

    val input3 = Coproduct[APBO](Banana(3))
    val expected3 = Coproduct[PBOA](Orange(3)) 
    
    val result3 = everywhere(cycleFruit)(input3)
    typed[PBOA](result3)
    assertEquals(expected3, result3)

    val input4 = Coproduct[APBO](Orange(4))
    val expected4 = Coproduct[PBOA](Apple(4)) 
    
    val result4 = everywhere(cycleFruit)(input4)
    typed[PBOA](result4)
    assertEquals(expected4, result4)
  }
  
  @Test
  def testList {
    val input: List[Apple] = List(Apple(1), Apple(2), Apple(3))
    val expected: List[String] = List("Pomme", "Pomme", "Pomme")
    
    val result = everywhere(showFruit)(input)
    typed[List[String]](result)
    assertEquals(expected, result)
    
    val input2: List[Apple] = List(Apple(1), Apple(1), Apple(1))
    val expected2: List[Pear] = List(Pear(1), Pear(1), Pear(1))
    
    val result2 = everywhere(cycleFruit)(input2)
    typed[List[Pear]](result2)
    assertEquals(expected2, result2)
  }

/*
  @Test
  def testCoproduct3 {
    // For this to work we will need to permute the mapped coproduct type back
    // into the order corresponding to the Generic for Fruit
  
//    val input: List[Fruit] = List(Apple(1), Pear(2), Banana(3), Orange(4))
//    val expected: List[Fruit] = List(Pear(1), Banana(2), Orange(3), Apple(4))
    
    val input: Option[Fruit] = Option(Apple(1))
    val expected: Option[Fruit] = Option(Pear(1))
    
    val result = everywhere(cycleFruit)(input)
    assertEquals(expected, result)
  }
*/

  @Test
  def testRecursion {
    val tree1: Tree[Int] = Leaf(1)
    val expected1: Tree[Int] = Leaf(2)
    
    val result1 = everywhere(inc)(tree1)
    typed[Tree[Int]](result1)
    assertEquals(expected1, result1)

    val tree2: Tree[Int] = Node(Leaf(1), Leaf(2))
    val expected2: Tree[Int] = Node(Leaf(2), Leaf(3))
    
    val result2 = everywhere(inc)(tree2)
    typed[Tree[Int]](result2)
    assertEquals(expected2, result2)

    val tree3: Tree[Int] = Node(Node(Node(Leaf(1), Node(Leaf(2), Leaf(3))), Leaf(4)), Node(Leaf(5), Leaf(6)))
    val expected3: Tree[Int] = Node(Node(Node(Leaf(2), Node(Leaf(3), Leaf(4))), Leaf(5)), Node(Leaf(6), Leaf(7)))

    val result3 = everywhere(inc)(tree3)
    typed[Tree[Int]](result3)
    assertEquals(expected3, result3)
  }
}
