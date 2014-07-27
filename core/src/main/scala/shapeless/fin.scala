/*
 * Copyright (c) 2011-14 Miles Sabin
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
 * Base trait for type level finite numbers, i.e. numbers less than some bound N
 */
trait Fin[N <: Succ[_]]

/**
 * Encoding of zero.
 */
case class FinZero[N <: Succ[_]]() extends Fin[N]

/**
 * Encoding of successor.
 */
case class FinSucc[N <: Succ[_], P <: Fin[N]]() extends Fin[Succ[N]]


object Fin {
  import ops.fin._

  def apply[M <: Nat, N <: Succ[_]]
    (implicit fromNat: FromNat[M, N]): fromNat.Out = fromNat()

  def apply[M <: Nat, N <: Succ[_]](m: M, n: N)
    (implicit fromNat: FromNat[m.N, n.N]): fromNat.Out = fromNat()

  def toNat[F <: Fin[_]](f: F)(implicit toNat: ToNat[F]): toNat.Out = toNat()
}
