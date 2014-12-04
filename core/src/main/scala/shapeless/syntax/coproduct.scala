/*
 * Copyright (c) 2013-14 Miles Sabin
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

  /**
   * Returns the head of this `Coproduct`
   */
  def head(implicit cc: IsCCons[C]): Option[cc.H] = cc.head(c)

  /**
   * Returns the tail of this `Coproduct`
   */

  def tail(implicit cc: IsCCons[C]): Option[cc.T] = cc.tail(c)

  /**
   * Returns the ''nth'' element of this `Coproduct`. An explicit type must be provided.
   * Available only if there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def at[N <: Nat](implicit at: At[C, N]): Option[at.A] = at(c)

  /**
   * Returns the ''nth'' element of this `Coproduct`.
   * Available only if there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def at(n: Nat)(implicit at: At[C, n.N]): Option[at.A] = at(c)

  /**
   * Returns the last element of this 'Coproduct'
   */
  def last(implicit il: InitLast[C]): Option[il.L] = il.last(c)

  /**
   * Returns all elements except the last
   */
  def init(implicit il: InitLast[C]): Option[il.I] = il.init(c)

  /**
   * Returns the first element of type `U` of this `Coproduct`. An explicit type argument must be provided. Available
   * only if there is evidence that this `Coproduct` has an element of type `U`.
   */
  def select[T](implicit selector: Selector[C, T]): Option[T] = selector(c)

  /**
   * Returns all elements of type `U` of this `Coproduct`. An explicit type argument must be provided.
   */
  def filter[U](implicit filter: Filter[C, U]): Option[filter.A]  = filter(c)

  /**
   * Returns all elements of type different than `U` of this `Coproduct`. An explicit type argument must be provided.
   */
  def filterNot[U](implicit filterNot: FilterNot[C, U]): Option[filterNot.A] = filterNot(c)

  /**
   * Returns the first element of type `U` of this `Coproduct` plus the remainder of the `Coproduct`.
   * An explicit type argument must be provided. Available only if there is evidence that this
   * `Coproduct` has an element of type `U`.
   */
  def removeElem[U](implicit remove: Remove[C, U]): Either[U, remove.Rest] = remove(c)

  /**
   * Returns the first element of type `U` of this `Coproduct` plus the remainder of the `Coproduct`.
   * An explicit type argument must be provided. Available only if there is evidence that this
   * `Coproduct` has an element of type `U`.
   */
  def removeElemC[U](implicit remove: Remove[C, U]): U :+: remove.Rest = remove.coproduct(c)


  /**
   * Splits this `Coproduct` at the ''nth'' element, returning the prefix and suffix as a pair. An explicit type
   * argument must be provided. Available only if there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def split[N <: Nat](implicit split: Split[C, N]): split.Out = split(c)
  def splitC[N <: Nat](implicit split: Split[C, N]): split.Left :+: split.Right :+: CNil = split.coproduct(c)

  /**
   * Splits this `Coproduct` at the ''nth'' element, returning the prefix and suffix as a pair. Available only if
   * there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def split(n: Nat)(implicit split: Split[C, n.N]): split.Out = split(c)
  def splitC(n: Nat)(implicit split: Split[C, n.N]): split.Left :+: split.Right :+: CNil = split.coproduct(c)


  /**
   * Takes the first `n` elements of this `Coproduct`. An explicit type argument must be provided. Available only if
   * there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def take[N <: Nat](implicit take: Take[C, N]): take.Out = take(c)

  /**
   * Takes the first `n` elements of this `Coproduct`. Available only if
   * there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def take(n: Nat)(implicit take: Take[C, n.N]): take.Out = take(c)
  
  /**
   * Drops the first `n` elements of this `Coproduct`. An explicit type argument must be provided. Available only if
   * there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def drop[N <: Nat](implicit drop: Drop[C, N]): drop.Out = drop(c)

  /**
   * Drops the first `n` elements of this `Coproduct`. Available only if
   * there is evidence that this `Coproduct` has at least ''n'' elements.
   */
  def drop(n: Nat)(implicit drop: Drop[C, n.N]): drop.Out = drop(c)

  /**
   * Permutes this `Coproduct` into the same order as another `Coproduct`. An explicit type argument must be supplied.
   * Available only if both `Coproduct`s have elements of the same types.
   */
  def align[K <: Coproduct](implicit align: Align[C, K]): K = align(c)

  /**
   * Permutes this `Coproduct` into the same order as another `Coproduct`. Available only if 
   * both `Coproduct`s have elements of the same types.
   */
  def align[K <: Coproduct](k: K)(implicit align: Align[C, K]): K = align(c)

  /**
   * Reverses this `Coproduct`.
   */
  def reverse(implicit reverse: Reverse[C]): reverse.Out = reverse(c)

  /**
   * Maps a higher rank function across this `Coproduct`.
   */
  def map(f: Poly)(implicit mapper: Mapper[f.type, C]): mapper.Out = mapper(c)

  /**
   * Flatmaps a higher rank function across this `Coproduct`.
   */
  def flatMap(op: Poly)(implicit flatMap: FlatMap[C, op.type]): flatMap.Out = flatMap(c)

  /**
   * Computes a fold over this `Coproduct` using the higher ranked function `f`. Available only if
   * there is evidence `f` can be applied all the elements of this `Coproduct`.
   */
  def fold(f: Poly)(implicit folder: Folder[f.type, C]): folder.Out = folder(c)

  /**
   * Returns an `Coproduct` typed as a repetition of the least upper bound of the types of the elements of
   * this `Coproduct`.
   */
  def unify(implicit unifier: Unifier[C]): unifier.Out = unifier(c)

  /**
   * Compute the length of this `Coproduct`.
   */
  def length(implicit length: Length[C]): length.Out = length()

  /**
   * Converts this `Coproduct` of values into a union with the provided keys.
   */
  def zipWithKeys[K <: HList](keys: K)(implicit zipWithKeys: ZipWithKeys[K, C]): zipWithKeys.Out = zipWithKeys(c)

  /**
   * Rotate this 'Coproduct' left by N. An explicit type argument must be provided.
   */
  def rotateLeft[N <: Nat](implicit rotateLeft: RotateLeft[C, N]): rotateLeft.Out = rotateLeft(c)

  /**
   * Rotate this 'Coproduct' left by `n`
   */
  def rotateLeft(n: Nat)(implicit rotateLeft: RotateLeft[C, n.N]): rotateLeft.Out = rotateLeft(c)

  /**
   * Rotate this 'Coproduct' right by N. An explicit type argument must be provided.
   */
  def rotateRight[N <: Nat](implicit rotateRight: RotateRight[C, N]): rotateRight.Out = rotateRight(c)

  /**
   * Rotate this 'Coproduct' right by `n`
   */
  def rotateRight(n: Nat)(implicit rotateRight: RotateRight[C, n.N]): rotateRight.Out = rotateRight(c)

  /**
   * Extend this `Coproduct` on the left.
   */
  def extendLeft[T]: T :+: C = Inr[T, C](c)

  /**
   * Extend this `Coproduct` on the right.
   */
  def extendRight[T](implicit extendRight: ExtendRight[C, T]): extendRight.Out = extendRight(c)

  /**
   * Extend this `Coproduct` on the left.
   */
  def extendLeftBy[K <: Coproduct](implicit extendLeftBy: ExtendLeftBy[K, C]): extendLeftBy.Out =
    extendLeftBy(c)

  /**
   * Extend this `Coproduct` on the right.
   */
  def extendRightBy[K <: Coproduct](implicit extendRightBy: ExtendRightBy[C, K]): extendRightBy.Out =
    extendRightBy(c)

  /**
   * Embeds this `Coproduct` into a "bigger" `Coproduct` if possible.
   */
  def embed[Super <: Coproduct](implicit basis: Basis[Super, C]): Super =
    basis.inverse(Right(c))

  /**
   * De-embeds a sub-`Coproduct` from this `Coproduct` if possible.
   */
  def deembed[Sub <: Coproduct](implicit basis: Basis[C, Sub]): basis.Out =
    basis(c)
}
