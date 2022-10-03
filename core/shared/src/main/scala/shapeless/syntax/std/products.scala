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
package std

object product {
  implicit def productOps[P <: Product](p: P): ProductOps[P] = new ProductOps[P](p)
}

final class ProductOps[P](val p: P) extends AnyVal {
  import ops.product._

  /**
   * Returns an `HList` containing the elements of this tuple.
   */
  def productElements(implicit gen: Generic[P]): gen.Repr = gen.to(p)

  /**
   * Compute the length of this product.
   */
  def length(implicit length : ProductLength[P]) : length.Out = length(p)

  /**
   * Returns a tuple containing the values of this product.
   */
  def toTuple[T](implicit toTuple: ToTuple.Aux[P, T]): T = toTuple(p)

  /**
   * Returns an `HList` containing the elements of this product.
   */
  def toHList[L <: HList](implicit toHList: ToHList.Aux[P, L]): L = toHList(p)

  /**
   * Returns a record containing the elements of this labelled product.
   */
  def toRecord[R <: HList](implicit toRecord: ToRecord.Aux[P, R]): R = toRecord(p)

  /**
   * Returns a collection `M` whose elements are typed as the Lub of the elements of this product.
   */
  def to[M[_]](implicit toTraversable: ToTraversable[P, M]): toTraversable.Out = toTraversable(p)

  /**
   * Returns a `Map` whose values are typed as the Lub of the values of this product.
   */
  def toMap[K, V](implicit toMap: ToMap.Aux[P, K, V]): Map[K, V] = toMap(p)

  /**
   * Returns a sized collection `M` whose elements are typed as the Lub of the elements of this product.
   */
  def toSized[M[_]](implicit toSized: ToSized[P, M]): toSized.Out = toSized(p)
}
