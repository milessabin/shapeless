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

import shapeless.test.IllTyper
import TypeOperators._

class TypeOperatorTests {
  object illTyped extends IllTyper(fail, _ => ())

  trait ATag
  
  object ATag {
    implicit def taggedToString[T](value: T with Tagged[ATag]): String = message
  
    val message = "This object has ATag tag type"
  }

  @Test
  def testImplicitScopeForTaggedType {
    val x = tag[ATag](1)
    val s: String = x
    assertEquals(ATag.message, s)
  }
  
  @Test
  def testNewtype {
    type MyString = Newtype[String, MyStringOps]
    
    def MyString(s : String) : MyString = newtype(s)
    
    case class MyStringOps(s : String) {
      def mySize = s.size
    }
    implicit val mkOps = MyStringOps
    
    val ms = MyString("foo")
    
    val env ="""
      import TypeOperators._
      type MyString = Newtype[String, MyStringOps]
      def MyString(s : String) : MyString = newtype(s)
      case class MyStringOps(s : String) {
        def mySize = s.size
      }
      implicit val mkOps = MyStringOps
      val ms = MyString("foo")
    """
    
    illTyped(env+"""
      val s: String = ms
    """)
    
    illTyped(env+"""
      val ms2: MyString = "foo"
    """)
    
    illTyped(env+"""
      ms.size
    """)
    
    assertEquals(3, ms.mySize)
    
    val s2 = "bar"
    val ms2 = MyString(s2)
    
    assertTrue(ms2 eq (s2 : AnyRef))
  }
}
