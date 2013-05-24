/*
 * Copyright (c) 2013 Miles Sabin 
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

import test.illTyped

class CoproductTests {
  def typed[T](t : => T) {}
  
  type ISB = Int :+: String :+: Boolean
  type III = Int :+: Int :+: Int
  
  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit
  
  type APB = Apple :+: Pear :+: Banana

  object size extends Poly1 {
    implicit val caseInt     = at[Int](_ => 1)
    implicit val caseString  = at[String](_.length)
    implicit val caseBoolean = at[Boolean](_ => 1)
  }

  @Test
  def testInject {
    implicitly[Inject[Int, Int]]
    implicitly[Inject[Int :+: Int, Int]]
    implicitly[Inject[Int :+: Int :+: Int, Int]]
    implicitly[Inject[String :+: Int, Int]]
    implicitly[Inject[Int :+: String, Int]]

    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)
  
    illTyped("""
      val foo4 = Coproduct[ISB](1.0)
    """)
  }
  
  @Test
  def testMatch {
    def cpMatch(v: ISB) = v match {
      case Inl(x) =>
        typed[Int](x)
      case Inr(Inl(x)) =>
        typed[String](x)
      case Inr(Inr(x)) =>
        typed[Boolean](x)
    }
    
    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)

    cpMatch(foo1)
    cpMatch(foo2)
    cpMatch(foo3)
  }
  
  @Test
  def testSelect {
    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)

    val sel1a = foo1.select[Int]
    typed[Option[Int]](sel1a)
    assertEquals(Some(23), sel1a)
    
    val sel1b = foo1.select[String]
    typed[Option[String]](sel1b)
    assertEquals(None, sel1b)
    
    val sel1c = foo1.select[Boolean]
    typed[Option[Boolean]](sel1c)
    assertEquals(None, sel1c)
    
    illTyped("""
      foo1.select[Double]
    """)
    
    val sel2a = foo2.select[Int]
    typed[Option[Int]](sel2a)
    assertEquals(None, sel2a)
    
    val sel2b = foo2.select[String]
    typed[Option[String]](sel2b)
    assertEquals(Some("foo"), sel2b)
    
    val sel2c = foo2.select[Boolean]
    typed[Option[Boolean]](sel2c)
    assertEquals(None, sel2c)
    
    illTyped("""
      foo2.select[Double]
    """)
    
    val sel3a = foo3.select[Int]
    typed[Option[Int]](sel3a)
    assertEquals(None, sel3a)
    
    val sel3b = foo3.select[String]
    typed[Option[String]](sel3b)
    assertEquals(None, sel3b)
    
    val sel3c = foo3.select[Boolean]
    typed[Option[Boolean]](sel3c)
    assertEquals(Some(true), sel3c)
    
    illTyped("""
      foo3.select[Double]
    """)
  }
  
  @Test
  def testMap {
    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)
    
    val foo1b = foo1 map size
    typed[III](foo1b)
    assertEquals(Inl(1), foo1b)
    
    val foo2b = foo2 map size
    typed[III](foo2b)
    assertEquals(Inr(Inl(3)), foo2b)
    
    val foo3b = foo3 map size
    typed[III](foo3b)
    assertEquals(Inr(Inr(1)), foo3b)
  }
  
  @Test
  def testUnify {
    val foo1 = Coproduct[ISB](23)
    val foo2 = Coproduct[ISB]("foo")
    val foo3 = Coproduct[ISB](true)
    
    val foo1b = foo1 map size
    val foo2b = foo2 map size
    val foo3b = foo3 map size

    val foo1c = foo1b.unify
    typed[Int](foo1c)
    assertEquals(1, foo1c)
    
    val foo2c = foo2b.unify
    typed[Int](foo2c)
    assertEquals(3, foo2c)
    
    val foo3c = foo3b.unify
    typed[Int](foo3c)
    assertEquals(1, foo3c)
    
    val f1 = Coproduct[APB](Apple())
    val f2 = Coproduct[APB](Pear())
    val f3 = Coproduct[APB](Banana())
    
    val f1b = f1.unify
    typed[Fruit](f1b)
    
    val f2b = f2.unify 
    typed[Fruit](f2b)
    
    val f3b = f3.unify 
    typed[Fruit](f3b)
  }
}