/*
 * Copyright (c) 2013 Miles Sabin 
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

/**
 * Carrier for `Coproduct` operations.
 * 
 * These methods are implemented here and extended onto the minimal `Coproduct` types to avoid issues that would
 * otherwise be caused by the covariance of `:+:[H, T]`.
 * 
 * @author Miles Sabin
 */
final class CoproductOps[C <: Coproduct](c: C) {
  import ops.coproduct._

  def map(f: Poly)(implicit mapper: Mapper[f.type, C]): mapper.Out = mapper(c)
  
  def select[T](implicit selector: Selector[C, T]): Option[T] = selector(c)

  /**
   * Returns the ''nth'' element of this `Coproduct`. An explicit type must be provided.
   * Available only if there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def at[N <: Nat](implicit at: At[C, N]): Option[at.A] = at(c)

  /**
   * Returns the ''nth'' element of this `Coproduct`.
   * Available only if there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def at[N <: Nat](n: N)(implicit at: At[C, n.N]): Option[at.A] = at(c)

  def unify(implicit unifier: Unifier[C]): unifier.Out = unifier(c)

  def zipWithKeys[K <: HList](keys: K)(implicit zipWithKeys: ZipWithKeys[K, C]): zipWithKeys.Out = zipWithKeys(keys, c)

  /**
   * Returns the head of this `Coproduct`
   */
  def head(implicit cc: IsCCons[C]): Option[cc.H] = cc.head(c)

  /**
   * Returns the tail of this `Coproduct`
   */
  def tail(implicit cc: IsCCons[C]): Option[cc.T] = cc.tail(c)

  /**
   * Returns all elements except the last
   */
  def init(implicit il: InitLast[C]): Option[il.I] = il.init(c)

  /**
   * Returns the last element of this 'Coproduct'
   */
  def last(implicit il: InitLast[C]): Option[il.L] = il.last(c)

  def length(implicit length: Length[C]): length.Out = length()

  def extendLeft[T]: T :+: C = Inr(c)
  def extendRight[T](implicit extendRight: ExtendRight[C, T]): extendRight.Out = extendRight(c)

  def rotateLeft[N <: Nat](implicit rotateLeft: RotateLeft[C, N]): rotateLeft.Out = rotateLeft(c)
  def rotateLeft[N <: Nat](n: N)(implicit rotateLeft: RotateLeft[C, n.N]): rotateLeft.Out = rotateLeft(c)

  def rotateRight[N <: Nat](implicit rotateRight: RotateRight[C, N]): rotateRight.Out = rotateRight(c)
  def rotateRight[N <: Nat](n: N)(implicit rotateRight: RotateRight[C, n.N]): rotateRight.Out = rotateRight(c)

  def reverse(implicit reverse: Reverse[C]): reverse.Out = reverse(c)
}
