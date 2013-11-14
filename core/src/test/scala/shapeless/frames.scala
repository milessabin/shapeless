/*
 * Copyright (c) 2013 Arseniy Zhizhelev
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

class FrameTests {
  import frame._

  def typed[T](t : => T) {}

  val intSlot1 = slot[Int](0)
  val intSlot2 = slot[Int](0)
  val stringSlot1 = slot[String]("")
  val stringSlot2 = slot[String]("")

  val frame1 = EmptyFrame `;` intSlot1
  
  case class FrameInstance1(fi:FrameInstance[frame1.type]) extends CustomFrameInstance[frame1.type](fi){
  	def intSlot1mul2 = fi(intSlot1)*2
  } 
  
  implicit val tframe1 = frame1.toTyped[FrameInstance1](FrameInstance1) 
  
  val r1 = tframe1.initialValue
  @Test
  def testGet {
    assertEquals(r1.intSlot1mul2, 0)
  }

  @Test
  def testSet {
    val r2 = r1.updated(intSlot1, 1)
    assertEquals(r2.intSlot1mul2, 2)
  }
  @Test
  def testGet2 {
    assertEquals((r1 >> intSlot1) .get, 0)
  }
  @Test
  def testSet2 {
    assertEquals(((r1 >> intSlot1) .set(1) >> intSlot1).get, 1)
  }
  
  val frame1Slot1 = slot[FrameInstance1](tframe1.initialValue)

  val frame2 = EmptyFrame `;` frame1Slot1
  
  case class FrameInstance2(fi:FrameInstance[frame2.type]) extends CustomFrameInstance[frame2.type](fi){
  } 
  
  implicit val tframe2 = frame2.toTyped[FrameInstance2](FrameInstance2) 

  val q1 = tframe2.initialValue
  @Test
  def testGet3 {
    assertEquals((q1 >> frame1Slot1 >> intSlot1) .get, 0)
  }
  
  @Test
  def testSet3 {
  	val q2 = (q1 >> frame1Slot1 >> intSlot1) .set(1)
    assertEquals((q2 >> frame1Slot1 >> intSlot1) .get, 1)
  }
}
