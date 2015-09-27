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

class NatTests {
  import nat._
  import ops.nat._
  
  trait Check[N <: Nat]
  def check(expected: Nat)(actually : => Check[expected.N]) {}
  
  @Test
  def testNat {
    implicitly[Succ[_1] =:= _2]
    
    implicitly[Pred.Aux[_19, _18]]
    
    def pred(n: Nat)(implicit pred : Pred[n.N]) = new Check[pred.Out] {}
    val pd1 = pred(19)
    check(18)(pd1)
    
    implicitly[Sum.Aux[_2, _3, _5]]
    
    def sum(a: Nat, b: Nat)(implicit sum : Sum[a.N, b.N]) = new Check[sum.Out] {}
    val s1 = sum(2, 3)
    check(5)(s1)

    implicitly[Diff.Aux[_5, _1, _4]]

    def diff(a: Nat, b: Nat)(implicit diff : Diff[a.N, b.N]) = new Check[diff.Out] {}
    val diff1 = diff(5, 1)
    check(4)(diff1)

    implicitly[Prod.Aux[_2, _3, _6]]
    implicitly[Prod.Aux[_4, _5, _20]]

    def prod(a: Nat, b: Nat)(implicit prod : Prod[a.N, b.N]) = new Check[prod.Out] {}
    val p1 = prod(2, 3)
    check(6)(p1)
    val p2 = prod(4, 5)
    check(20)(p2)

    implicitly[Div.Aux[_7, _2, _3]]
    implicitly[Div.Aux[_22, _11, _2]]
    implicitly[Div.Aux[_15, _3, _5]]

    def div(a: Nat, b: Nat)(implicit div : Div[a.N, b.N]) = new Check[div.Out] {}
    val d1 = div(7, 2)
    check(3)(d1)
    val d2 = div(22, 11)
    check(2)(d2)
    val d3 = div(15, 3)
    check(5)(d3)

    implicitly[Mod.Aux[_7, _2, _1]]
    implicitly[Mod.Aux[_22, _5, _2]]
    implicitly[Mod.Aux[_9, _3, _0]]

    def mod(a: Nat, b: Nat)(implicit mod : Mod[a.N, b.N]) = new Check[mod.Out] {}
    val m1 = mod(7, 2)
    check(1)(m1)
    val m2 = mod(22, 5)
    check(2)(m2)
    val m3 = mod(9, 3)
    check(0)(m3)

    implicitly[LT[_3, _5]]
    implicitly[LT[_10, _15]]
    implicitly[LTEq[_2, _2]]
    implicitly[LTEq[_2, _3]]

    implicitly[Min.Aux[_0, _0, _0]]
    implicitly[Min.Aux[_5, _2, _2]]
    implicitly[Min.Aux[_3, _8, _3]]

    def min[A <: Nat, B <: Nat](implicit min : Min[A, B]) = new Check[min.Out] {}
    val min1 = min[_3, _4]
    check(3)(min1)
    val min2 = min[_5, _4]
    check(4)(min2)

    implicitly[Max.Aux[_0, _0, _0]]
    implicitly[Max.Aux[_5, _2, _5]]
    implicitly[Max.Aux[_3, _8, _8]]

    def max[A <: Nat, B <: Nat](implicit max : Max[A, B]) = new Check[max.Out] {}
    val max1 = max[_3, _4]
    check(4)(max1)
    val max2 = max[_5, _4]
    check(5)(max2)

    implicitly[Pow.Aux[_0, _8, _1]]
    implicitly[Pow.Aux[_9, _0, _0]]
    implicitly[Pow.Aux[_3, _2, _8]]

    def pow[A <: Nat, B <: Nat](implicit pow : Pow[A, B]) = new Check[pow.Out] {}
    val e1 = pow[_3, _1]
    check(1)(e1)
    val e2 = pow[_2, _3]
    check(9)(e2)
    val e3 = pow[_2, _4]
    check(16)(e3)

    implicitly[Range.Aux[_0,_0, HNil]]
    implicitly[Range.Aux[_0,_2, _0::_1::HNil]]
    implicitly[Range.Aux[_1,_1, HNil]]
    implicitly[Range.Aux[_1,_2,_1::HNil]]
    implicitly[Range.Aux[_1,_4, _1::_2::_3::HNil]]

    val r1 = the[Range[_0,_0]]
    val r2 = the[Range[_0,_1]]
    val r3 = the[Range[_1,_1]]
    val r4 = the[Range[_1,_5]]

    import shapeless.testutil._

    assertTypedEquals[HNil](HNil, r1())
    assertTypedEquals[_0::HNil](_0::HNil, r2())
    assertTypedEquals[HNil](HNil, r3())
    assertTypedEquals[_1::_2::_3::_4::HNil](_1::_2::_3::_4::HNil, r4())

    // GCD tests

    implicitly[GCD.Aux[_0, _0, _0]]
    implicitly[GCD.Aux[_0, _1, _1]]
    implicitly[GCD.Aux[_1, _0, _1]]
    implicitly[GCD.Aux[_21, _14, _7]]
    implicitly[GCD.Aux[_20, _10, _10]]

    // LCM tests
    implicitly[LCM.Aux[_0, _1, _0]]
    implicitly[LCM.Aux[_1, _0, _0]]
    implicitly[LCM.Aux[_2, _3, _6]]
    implicitly[LCM.Aux[_4, _6, _12]]
    implicitly[LCM.Aux[_3, _7, _21]]

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
