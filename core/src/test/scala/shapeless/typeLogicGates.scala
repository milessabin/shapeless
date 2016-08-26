/*
 * Copyright (c) 2014 Miles Sabin
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

import test._
import ops.nat.LT._
import nat._

import shapeless.test.illTyped

class TypeLogicGatesTests {
  @Test
  def testOr {
    val ev1 = implicitly[(_1 < _2) || (_3 < _5)]
    assert(ev1.a.isDefined && ev1.b.isDefined)
    val ev2 = implicitly[(_1 < _3) || (_2 < _1)]
    assert(ev2.a.isDefined && ev2.b.isEmpty)
    val ev3 = implicitly[(_3 < _1) || (_2 < _3)]
    assert(ev3.a.isEmpty && ev3.b.isDefined)
    illTyped("""
      implicitly[(_3 < _1) || (_4 < _2)]
    """)
  }

  @Test
  def testAnd {
    implicitly[(_3 < _4) && (_4 < _5)]
    illTyped("""
      implicitly[(_1 < _2) && (_3 < _2)]
    """)
  }

  @Test
  def testXor {
    val ev1 = implicitly[(_2 < _4) ^^ (_3 < _1)]
    assert(ev1.a.isDefined && ev1.b.isEmpty)
    val ev2 = implicitly[(_3 < _2) ^^ (_1 < _2)]
    assert(ev2.a.isEmpty && ev2.b.isDefined)
    illTyped("""
      implicitly[(_1 < _2) ^^ (_1 < _3)]
    """)
  }
 
  @Test
  def testNot {
    implicitly[!![_4 < _2]]
    illTyped("""
      implicitly[!![_1 < _2]]
    """)
  }
}
