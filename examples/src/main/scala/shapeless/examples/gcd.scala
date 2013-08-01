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

package shapeless.examples

/**
 * Type-level encoding of GCD.
 * 
 * @author George Leontiev
 */
/*
object GCDExamples {
  import shapeless._
  import nat._

  def typed[T](t : => T) {}

  trait GCD[X <: Nat, Y <: Nat] {
    type Z <: Nat
  }

  object GCD {
    implicit def gcd1[X <: Nat, Y <: Nat, Z0 <: Nat](implicit gcd : GCDAux[X, Y, Z0]) = new GCD[X, Y] {
      type Z = Z0
    }
    def gcd[N <: Nat](x : Nat, y : Nat)(implicit gcd : GCDAux[x.N, y.N, N], wn : WitnessAux[N]) = wn.value
  }

  trait GCDAux[X <: Nat, Y <: Nat, Z <: Nat]

  object GCDAux {
    implicit def gcd0[X <: Nat](implicit ev : LT[_0, X]) = new GCDAux[X, _0, X] {}
    implicit def gcd1[X <: Nat, Y <: Nat, Z <: Nat, Out <: Nat]
      (implicit ev0 : ModAux[X, Y, Z], ev1 : GCDAux[Y, Z, Out]) = new GCDAux[X, Y, Out] {}
  }

  import GCD._

  val g1 = gcd(2, 3)
  typed[_1](g1)

  val g2 = gcd(4, 10)
  typed[_2](g2)

  val g3 = gcd(15, 6)
  typed[_3](g3)
}
*/
