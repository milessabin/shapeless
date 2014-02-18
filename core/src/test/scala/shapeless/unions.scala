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
import org.junit.Assert._

class UnionTests {
  import record.RecordType
  import union._
  import syntax.singleton._
  import test._

  val uSchema = RecordType.like('i ->> 23 :: 's ->> "foo" :: 'b ->> true :: HNil)
  type U = uSchema.Union

  @Test
  def testGetLiterals {
    val u1 = Coproduct[U]('i ->> 23)
    val u2 = Coproduct[U]('s ->> "foo")
    val u3 = Coproduct[U]('b ->> true)
    
    val v1 = u1.get('i)
    typed[Option[Int]](v1)
    assertEquals(Some(23), v1)

    val v2 = u2.get('s)
    typed[Option[String]](v2)
    assertEquals(Some("foo"), v2)

    val v3 = u3.get('b)
    typed[Option[Boolean]](v3)
    assertEquals(Some(true), v3)

    illTyped("""
      u1.get('foo)
    """)
  }
}
