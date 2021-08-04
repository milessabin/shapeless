/*
 * Copyright (c) 2011-16 Miles Sabin
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

import scala.compiletime.ops.int.S

trait NatScalaCompat {
  type NatToInt[N <: Nat] <: Int = N match {
    case _0      => 0
    case Succ[n] => scala.compiletime.ops.int.S[NatToInt[n]]
  }

  type IntToNat[N <: Int] <: Nat = N match {
    case 0 => _0
    case S[n] => Succ[IntToNat[n]]
  }

  //Transparent gives better types here
  transparent inline implicit def apply(inline i: Int): Nat =
    if i < 0 then compiletime.error("Can't convert value less than 0 to nat")
    else if i == 0 then (new _0).asInstanceOf[IntToNat[i.type]]
    else (new Succ).asInstanceOf[IntToNat[i.type]]
}

trait NatWithTypeAtPosScalaCompat {

  //Transparent gives better types here
  transparent inline implicit def fromIntList[L <: HList, Out](inline i: Int)(implicit at: ops.hlist.At.Aux[L, Nat.IntToNat[i.type], Out]): NatWithTypeAtPos[L] =
    if i < 0 then compiletime.error("Can't convert value less than 0 to nat")
    else {
      type N0 = Nat.IntToNat[i.type]
      val n = Nat(i)

      new NatWithTypeAtPos[L] {
        type N = N0
        type Tpe = Out
        val value: N = n.asInstanceOf[N]
      }
    }

  //Transparent gives better types here
  transparent inline implicit def fromIntTuple[T <: scala.Tuple, Out](inline i: Int)(implicit at: ops.tuple.At.Aux[T, Nat.IntToNat[i.type], Out]): NatWithTypeAtPos[T] =
    if i < 0 then compiletime.error("Can't convert value less than 0 to nat")
    else {
      type N0 = Nat.IntToNat[i.type]
      val n = Nat(i)

      new NatWithTypeAtPos[T] {
        type N = N0
        type Tpe = Out
        val value: N = n.asInstanceOf[N]
      }
    }
}
