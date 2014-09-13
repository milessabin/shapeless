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

class NumberTests {
  import rint._
  import ops.numbers._
  
  trait Check[N <: Number]
  def check(expected: Number)(actually : => Check[expected.N]) {}
  
  @Test
  def testNat {
    implicitly[Succ[_1] =:= _2]
    
    implicitly[Predecessor.Aux[_m18, _m19]]
    
    def pred(n: RInt)(implicit pred : Predecessor[n.N]) = new Check[pred.Out] {}
    val pd1 = pred(-18)
    check(-19)(pd1)

    // Type level
    assertEquals(-1, toInt[_m1])
    assertEquals(-2, toInt[_m2])
    assertEquals(-3, toInt[_m3])
    assertEquals(-4, toInt[_m4])
    assertEquals(-5, toInt[_m5])
    assertEquals(-6, toInt[_m6])
    assertEquals(-7, toInt[_m7])
    assertEquals(-8, toInt[_m8])
    assertEquals(-9, toInt[_m9])
    assertEquals(-10, toInt[_m10])
    assertEquals(-11, toInt[_m11])
    assertEquals(-12, toInt[_m12])
    assertEquals(-13, toInt[_m13])
    assertEquals(-14, toInt[_m14])
    assertEquals(-15, toInt[_m15])
    assertEquals(-16, toInt[_m16])
    assertEquals(-17, toInt[_m17])
    assertEquals(-18, toInt[_m18])
    assertEquals(-19, toInt[_m19])
    assertEquals(-20, toInt[_m20])
    assertEquals(-21, toInt[_m21])
    assertEquals(-22, toInt[_m22])

    // Value level
    assertEquals(-1, toInt(_m1))
    assertEquals(-2, toInt(_m2))
    assertEquals(-3, toInt(_m3))
    assertEquals(-4, toInt(_m4))
    assertEquals(-5, toInt(_m5))
    assertEquals(-6, toInt(_m6))
    assertEquals(-7, toInt(_m7))
    assertEquals(-8, toInt(_m8))
    assertEquals(-9, toInt(_m9))
    assertEquals(-10, toInt(_m10))
    assertEquals(-11, toInt(_m11))
    assertEquals(-12, toInt(_m12))
    assertEquals(-13, toInt(_m13))
    assertEquals(-14, toInt(_m14))
    assertEquals(-15, toInt(_m15))
    assertEquals(-16, toInt(_m16))
    assertEquals(-17, toInt(_m17))
    assertEquals(-18, toInt(_m18))
    assertEquals(-19, toInt(_m19))
    assertEquals(-20, toInt(_m20))
    assertEquals(-21, toInt(_m21))
    assertEquals(-22, toInt(_m22))
  }
}
