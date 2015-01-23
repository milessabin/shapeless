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
    val pd2 = pred(0)
    check(-1)(pd2)
    val pd3 = pred(18)
    check(17)(pd3)

    implicitly[Successor.Aux[_m18, _m17]]

    def succ(n: RInt)(implicit succ : Successor[n.N]) = new Check[succ.Out] {}
    val sc1 = succ(-18)
    check(-17)(sc1)
    val sc2 = succ(-1)
    check(0)(sc2)
    val sc3 = succ(3)
    check(4)(sc3)

    implicitly[Sum.Aux[_m5, _1, _m4]]
    implicitly[Sum.Aux[_m5, _m1, _m6]]
    implicitly[Sum.Aux[_1,_0,_1]]

    def sum(a: RInt, b: RInt)(implicit sum : Sum[a.N, b.N]) = new Check[sum.Out] {}
    val s1 = sum(2, 3)
    check(5)(s1)
    val s2 = sum(-3, 5)
    check(2)(s2)
    val s3 = sum(-4,-4)
    check(-8)(s3)

    implicitly[Diff.Aux[_m5, _1, _m6]]
    implicitly[Diff.Aux[_m5, _m1, _m4]]

    def diff(a: RInt, b: RInt)(implicit diff : Diff[a.N, b.N]) = new Check[diff.Out] {}
    val d1 = diff(2, 3)
    check(-1)(d1)
    val d2 = diff(-3, 5)
    check(-8)(d2)
    val d3 = diff(-4,-4)
    check(0)(d3)
    val d4 = diff(0,0)
    check(0)(d4)

    implicitly[Prod.Aux[_m5, _2, _m10]]
    implicitly[Prod.Aux[_5, _2, _10]]
    implicitly[Prod.Aux[_5, _m1, _m5]]
    implicitly[Prod.Aux[_m4, _m2, _8]]
    implicitly[Prod.Aux[_m1, _0, _0]]

    def prod(a: RInt, b: RInt)(implicit prod : Prod[a.N, b.N]) = new Check[prod.Out] {}
    val p1 = prod(2, -3)
    check(-6)(p1)
    val p2 = prod(-4, -5)
    check(20)(p2)
    val p3 = prod(-1, 0)
    check(0)(p3)
    val p4 = prod(0, -1)
    check(0)(p4)

    implicitly[LT[_0,_1]]
    implicitly[LT[_4,_8]]
    implicitly[LT[_m4, _m2]]
    implicitly[LT[_m4, _4]]

    implicitly[LTEq[_0,_1]]
    implicitly[LTEq[_4,_8]]
    implicitly[LTEq[_m4, _m2]]
    implicitly[LTEq[_m4, _4]]
    implicitly[LTEq[_m1, _m1]]
    implicitly[LTEq[_2, _2]]


    implicitly[Div.Aux[_2, _1, _2]]
    implicitly[Div.Aux[_4, _m2, _m2]]
    implicitly[Div.Aux[_m4, _m2, _2]]
    implicitly[Div.Aux[_m4, _2, _m2]]
    def div(a: RInt, b: RInt)(implicit div : Div[a.N, b.N]) = new Check[div.Out] {}
    val div1 = div(4,-2)
    check(-2)(div1)
    val div2 = div(-10,3)
    check(-3)(div2)
    val div3 = div(-5, -2)
    check(2)(div3)
    val div4 = div(-10, 11)
    check(0)(div4)
    val div5 = div(7, 9)
    check(0)(div5)
    val div6 = div(15, -5)
    check(-3)(div6)

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
