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

import org.junit.Test
import org.junit.Assert._

class PolyFunTests {
  import PolyFun._
  
  def typed[T](t : => T) {}

  object toInt extends (Id ~> Const[Int]#λ) {
    def default[T](t : T) = t.toString.toInt
  }
  
  object size extends (Id ~> Const[Int]#λ) {
    def default[T](t : T) = 1
  }
  implicit def sizeInt = size.λ[Int](x => 1)
  implicit def sizeString = size.λ[String](s => s.length)
  implicit def sizeList[T] = size.λ[List[T]](l => l.length)
  implicit def sizeOption[T](implicit cases : size.λ[T]) = size.λ[Option[T]](t => 1+size(t.get))
  implicit def sizeTuple[T, U](implicit st : size.λ[T], su : size.λ[U]) = size.λ[(T, U)](t => size(t._1)+size(t._2))
  
  @Test
  def testPolyFun {
    implicitly[choose.λ[Int]]
    implicitly[Case[choose.type, Set[Int] => Option[Int]]]
    
    implicitly[size.λ[Int]]
    implicitly[Case[size.type, Option[Int] => Int]]

    implicitly[option.λ[Int]]
    implicitly[Case[option.type, Int => Option[Int]]]
    implicitly[Case[option.type, Id[Int] => Option[Int]]]

    implicitly[singleton.λ[Int]]
    implicitly[Case[singleton.type, Int => Set[Int]]]
    implicitly[Case[singleton.type, Id[Int] => Set[Int]]]

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
    
    val a3 = pairApply[Const[Int]#λ](size)
    typed[(Int, Int)](a3)
    assertEquals((1, 1), a3)  // size without type specific cases
    
    // Use as polymorphic function values with type specific cases
    def pairApply2[G[_]](f : Id ~> G)(implicit  fi : f.λ[Int], fs : f.λ[String]) = (f(23), f("foo"))
    
    val a4 = pairApply2(singleton)
    typed[(Set[Int], Set[String])](a4)
    assertEquals((Set(23), Set("foo")), a4)
    
    val a5 = pairApply2(list)
    typed[(List[Int], List[String])](a5)
    assertEquals((List(23), List("foo")), a5)
    
    val a6 = pairApply2[Const[Int]#λ](size)
    typed[(Int, Int)](a6)
    assertEquals((1, 3), a6)

    def pairMap[F[_]](f : Id ~> F) = (List(1, 2, 3) map f, List("foo", "bar", "baz") map f)
  
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

    import HList._
    
    val l8 = 23 :: "foo" :: List(1, 2, 3, 4) :: Option("bar") :: (23, "foo") :: 2.0 :: HNil
    val l9 = l8 map size
    typed[Int :: Int :: Int :: Int :: Int :: Int :: HNil](l9)
    assertEquals(1 :: 3 :: 4 :: 4 :: 4 :: 1 :: HNil, l9)
  }
  
  @Test
  def testPolyVal {
    import PolyFun._

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
}
