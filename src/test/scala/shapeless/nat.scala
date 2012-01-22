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

class NatTests {
  import Nat._
  
  trait Check[N <: Nat]
  def check[N <: Nat](n : => Check[N]) {}
  
  @Test
  def testNat {
    implicitly[Succ[_1] =:= _2]
    
    implicitly[PredAux[_19, _18]]
    
    def pred[A <: Nat](implicit pred : Pred[A]) = new Check[pred.Out] {}
    val pd1 = pred[_19]
    check[_18](pd1)
    
    implicitly[SumAux[_2, _3, _5]]
    
    def sum[A <: Nat, B <: Nat](implicit sum : Sum[A, B]) = new Check[sum.Out] {}
    val s1 = sum[_2, _3]
    check[_5](s1)
    
    implicitly[ProdAux[_2, _3, _6]]
    implicitly[ProdAux[_4, _5, _20]]
    
    def prod[A <: Nat, B <: Nat](implicit prod : Prod[A, B]) = new Check[prod.Out] {}
    val p1 = prod[_2, _3]
    check[_6](p1)
    val p2 = prod[_4, _5]
    check[_20](p2)

    // Type level
    assertEquals(0, toInt[_0])
    assertEquals(1, toInt[_1])
    assertEquals(2, toInt[_2])
    assertEquals(3, toInt[_3])
    assertEquals(4, toInt[_4])
    assertEquals(5, toInt[_5])
    assertEquals(6, toInt[_6])
    assertEquals(7, toInt[_7])
    assertEquals(8, toInt[_8])
    assertEquals(9, toInt[_9])
    assertEquals(10, toInt[_10])
    assertEquals(11, toInt[_11])
    assertEquals(12, toInt[_12])
    assertEquals(13, toInt[_13])
    assertEquals(14, toInt[_14])
    assertEquals(15, toInt[_15])
    assertEquals(16, toInt[_16])
    assertEquals(17, toInt[_17])
    assertEquals(18, toInt[_18])
    assertEquals(19, toInt[_19])
    assertEquals(20, toInt[_20])
    assertEquals(21, toInt[_21])
    assertEquals(22, toInt[_22])

    // Value level
    assertEquals(0, toInt(_0))
    assertEquals(1, toInt(_1))
    assertEquals(2, toInt(_2))
    assertEquals(3, toInt(_3))
    assertEquals(4, toInt(_4))
    assertEquals(5, toInt(_5))
    assertEquals(6, toInt(_6))
    assertEquals(7, toInt(_7))
    assertEquals(8, toInt(_8))
    assertEquals(9, toInt(_9))
    assertEquals(10, toInt(_10))
    assertEquals(11, toInt(_11))
    assertEquals(12, toInt(_12))
    assertEquals(13, toInt(_13))
    assertEquals(14, toInt(_14))
    assertEquals(15, toInt(_15))
    assertEquals(16, toInt(_16))
    assertEquals(17, toInt(_17))
    assertEquals(18, toInt(_18))
    assertEquals(19, toInt(_19))
    assertEquals(20, toInt(_20))
    assertEquals(21, toInt(_21))
    assertEquals(22, toInt(_22))
  }
}
