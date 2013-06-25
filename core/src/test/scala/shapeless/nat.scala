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

    implicitly[DivAux[_7, _2, _3]]
    implicitly[DivAux[_22, _11, _2]]
    implicitly[DivAux[_15, _3, _5]]

    def div[A <: Nat, B <: Nat](implicit div : Div[A, B]) = new Check[div.Out] {}
    val d1 = div[_7, _2]
    check[_3](d1)
    val d2 = div[_22, _11]
    check[_2](d2)
    val d3 = div[_15, _3]
    check[_5](d3)

    implicitly[ModAux[_7, _2, _1]]
    implicitly[ModAux[_22, _5, _2]]
    implicitly[ModAux[_9, _3, _0]]

    def mod[A <: Nat, B <: Nat](implicit mod : Mod[A, B]) = new Check[mod.Out] {}
    val m1 = mod[_7, _2]
    check[_1](m1)
    val m2 = mod[_22, _5]
    check[_2](m2)
    val m3 = mod[_9, _3]
    check[_0](m3)

    implicitly[PowAux[_0, _8, _1]]
    implicitly[PowAux[_9, _0, _0]]
    implicitly[PowAux[_3, _2, _8]]

    def pow[A <: Nat, B <: Nat](implicit pow : Pow[A, B]) = new Check[pow.Out] {}
    val e1 = pow[_3, _1]
    check[_1](e1)
    val e2 = pow[_2, _3]
    check[_9](e2)
    val e3 = pow[_2, _4]
    check[_16](e3)

    implicitly[MinAux[_0, _0, _0]]
    implicitly[MinAux[_5, _2, _2]]
    implicitly[MinAux[_3, _8, _3]]

    def min[A <: Nat, B <: Nat](implicit min : Min[A, B]) = new Check[min.Out] {}
    val min1 = min[_3, _4]
    check[_3](min1)
    val min2 = min[_5, _4]
    check[_4](min2)

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
