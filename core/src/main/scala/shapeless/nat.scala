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

/**
 * Base trait for type level natural numbers.
 *
 * @author Miles Sabin
 */
trait Nat {
  type N <: Nat
}

/**
 * Encoding of successor.
 *
 * @author Miles Sabin
 */
case class Succ[P <: Nat]() extends Nat {
  type N = Succ[P]
}

/**
 * Encoding of zero.
 *
 * @author Miles Sabin
 */
class _0 extends Nat with Serializable {
  type N = _0
}

/**
 * Type level encoding of the natural numbers.
 *
 * @author Miles Sabin
 */
object Nat extends Nats with NatScalaCompat {
  import ops.nat._
  import syntax.NatOps

  /** The natural number 0 */
  type _0 = shapeless._0
  val _0: _0 = new _0

  def toInt[N <: Nat](implicit toIntN : ToInt[N]) = toIntN()

  def toInt(n : Nat)(implicit toIntN : ToInt[n.N]) = toIntN()

  implicit def natOps[N <: Nat](n : N) : NatOps[N] = new NatOps(n)
}
