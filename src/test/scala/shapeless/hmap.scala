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

class HMapTests {
  import HList._
  import Mapper._
  import MapperAux._
  
  def typed[T](t : => T) {}

  @Test
  def testHMap {
    class Rel[K, V]
    implicit val intToString = new Rel[Int, String]
    implicit val stringToInt = new Rel[String, Int]
    
    val hm = new HMap[Rel]+(23 -> "foo")+("bar" -> 13)
    
    val s1 = hm.get(23)
    assertTrue(isDefined(s1))
    typed[Option[String]](s1)
    assertEquals(Some("foo"), s1)

    val i1 = hm.get("bar")
    assertTrue(isDefined(i1))
    typed[Option[Int]](i1)
    assertEquals(Some(13), i1)
    
    import hm._
    
    // Map over an HList
    val l1 = 23 :: "bar" :: 23 :: "bar" :: HNil
    val l2 = l1 map hm
    typed[String :: Int :: String :: Int :: HNil](l2)
    assertEquals("foo" :: 13 :: "foo" :: 13 :: HNil, l2)
    
    // Use as an argument to a HoF
    def pairApply[F <: Poly](f : F)(implicit ci : f.Case1[Int], cs : f.Case1[String]) = (f(23), f("bar"))

    val a1 = pairApply(hm)
    typed[(String, Int)](a1)
    assertEquals(("foo", 13), a1)
  }
}