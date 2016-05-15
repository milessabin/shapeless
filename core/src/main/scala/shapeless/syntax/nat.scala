/*
 * Copyright (c) 2011-16 Dale Wijnand
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
package syntax

object nat {

  import ops.nat.BoundedRange
  import BoundedRange.{Exclusive, Inclusive}

  type *--*[A,B] = BoundedRange[Inclusive[A], Inclusive[B]]

  type :--:[A,B] = BoundedRange[Exclusive[A], Exclusive[B]]

  type :--*[A,B] = BoundedRange[Exclusive[A], Inclusive[B]]

  type *--:[A,B] = BoundedRange[Inclusive[A], Exclusive[B]]

}

/**
 * Carrier for `Nat` operations.
 *
 * @author Dale Wijnand
 */
final class NatOps[N <: Nat](val n: N) extends AnyVal with Serializable {
  import ops.nat._

  /**
   * Returns the int value of this `Nat`.
   */
  def toInt(implicit toIntN: ToInt[n.N]): Int = Nat.toInt(n)
}
