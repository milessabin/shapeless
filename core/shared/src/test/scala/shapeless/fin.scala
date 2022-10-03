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

import org.junit.Test
import org.junit.Assert._

import test._
import testutil._


class FinTests {
  import nat._
  import fin._
  import ops.fin._

  @Test
  def testFromNats: Unit = {
    val r1 = Fin[_0, _1]
    assertTypedEquals[FinZero[_1]](FinZero[_1](), r1)

    val r2 = Fin(_0, _1)
    assertTypedEquals[FinZero[_1]](FinZero[_1](), r2)

    illTyped("""
      Fin[_0, _0]
    """)

    illTyped("""
      Fin(_0, _0)
    """)
  }

  @Test
  def testToNat: Unit = {
    val r1 = Fin.toNat(Fin[_0, _3])
    assertTypedEquals[_0](_0, r1)

    val r2 = Fin.toNat(Fin[_1, _3])
    assertTypedEquals[_1](_1, r2)

    val r3 = Fin.toNat(Fin[_2, _3])
    assertTypedEquals[_2](_2, r3)
  }
}
