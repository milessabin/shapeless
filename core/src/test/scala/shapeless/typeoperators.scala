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

import TypeOperators._

class TypeOperatorTests {
  import org.junit.Test
  import org.junit.Assert._


  @Test
  def testImplicitScopeForTaggedType {
    val x = tag[ATag](1)
    val s: String = x
    assertEquals(ATag.message, s)
  }
}

trait ATag
object ATag {

  val message = "This object has ATag tag type"

  implicit def taggedToString[T](value: T with Tagged[ATag]): String = message
}
