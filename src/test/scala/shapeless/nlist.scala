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

object NListTests {
  import Nat._
  import NList._
  
  def typed[T](t : => T) {}
  
  @Test
  def testBasics {
    val l0 = List.empty[Int]
    val l1 = List(1)
    val l2 = List(1, 2)
    val l3 = List(1, 2, 3)
    
    val nl0 = l0.toNList[_0]
    assertTrue(nl0.isDefined)
    val nl0b = l0.toNList[_1]
    assertTrue(nl0b.isEmpty)
    val nl0c = l0.toNList[_2]
    assertTrue(nl0c.isEmpty)
    val nl0d = l0.toNList[_3]
    assertTrue(nl0d.isEmpty)
    
    //val h0 = nl0.get.head
    //val t0 = nl0.get.tail
    
    val nl1 = l1.toNList[_0]
    assertTrue(nl1.isEmpty)
    val nl1b = l1.toNList[_1]
    assertTrue(nl1b.isDefined)
    val nl1c = l1.toNList[_2]
    assertTrue(nl1c.isEmpty)
    val nl1d = l1.toNList[_3]
    assertTrue(nl1d.isEmpty)

    val h1 = nl1b.get.head
    val t1 = nl1b.get.tail
    //val t1b = nl1b.get.tail.tail

    val nl2 = l2.toNList[_0]
    assertTrue(nl2.isEmpty)
    val nl2b = l2.toNList[_1]
    assertTrue(nl2b.isEmpty)
    val nl2c = l2.toNList[_2]
    assertTrue(nl2c.isDefined)
    val nl2d = l2.toNList[_3]
    assertTrue(nl2d.isEmpty)
    
    val h2 = nl2c.get.head
    val t2 = nl2c.get.tail
    val t2b = nl2c.get.tail.tail
    //val t2c = nl1c.get.tail.tail.tail

    val nl3 = l3.toNList[_0]
    assertTrue(nl2.isEmpty)
    val nl3b = l3.toNList[_1]
    assertTrue(nl3b.isEmpty)
    val nl3c = l3.toNList[_2]
    assertTrue(nl3c.isEmpty)
    val nl3d = l3.toNList[_3]
    assertTrue(nl3d.isDefined)

    val h3 = nl3d.get.head
    val t3 = nl3d.get.tail
    val t3b = nl3d.get.tail.tail
    val t3c = nl3d.get.tail.tail.tail
    //val t3d = nl1d.get.tail.tail.tail.tail
    
    val rs = "foo".toNList[_3].get.toRepr
    
    val rl = List(1, 2, 3).toNList[_3].get.toRepr
    
    val s1 = "foo".toNList[_3]
    assertTrue(s1.isDefined)
    val s2 = "bar".toNList[_3]
    assertTrue(s2.isDefined)

    val s3 = s1.get ++ s2.get
    assertEquals("foobar", s3.toRepr)
    
    val cs = for(x <- s1 ; y <- s2) yield x ++ y
    assertEquals("foobar", cs.get.toRepr)
    
    val ll1 = List(1, 2, 3).toNList[_3]
    assertTrue(ll1.isDefined)
    val ll2 = List(4, 5, 6).toNList[_3]
    assertTrue(ll2.isDefined)

    val ll3 = ll1.get ++ ll2.get
    assertEquals(List(1, 2, 3, 4, 5, 6), ll3.toRepr)
    
    val cl = for(x <- ll1 ; y <- ll2) yield x ++ y
    assertEquals(List(1, 2, 3, 4, 5, 6), cl.get.toRepr)
  }
}
