/*
 * Copyright (c) 2011-15 Miles Sabin 
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

import scala.language.dynamics
import tag.@@

/**
 * Discriminated union operations on `Coproducts`'s with field-like elements.
 * 
 * @author Miles Sabin
 */
final class UnionOps[C <: Coproduct](val c : C) extends AnyVal with Serializable {
  import ops.union._

  /**
   * Returns the value associated with the singleton typed key k. Only available if this union has a field with
   * with keyType equal to the singleton type k.T.
   */
  def get(k: Witness)(implicit selector : Selector[C, k.T]): selector.Out = selector(c)
  
  /**
   * Returns the value associated with the singleton typed key k. Only available if this union has a field with
   * with keyType equal to the singleton type k.T.
   *
   * Note that this can creates a bogus ambiguity with `CoproductOps#apply` as described in
   * https://issues.scala-lang.org/browse/SI-5142. If this method is accessible the conflict can be worked around by
   * using CoproductOps#at instead of `CoproductOps#apply`.
   */
  def apply(k: Witness)(implicit selector : Selector[C, k.T]): selector.Out = selector(c)

  /**
   * Returns the keys of this union as an `HList` of singleton typed values.
   */
  def keys(implicit keys: Keys[C]): keys.Out = keys()

  /**
   * Returns a `Coproduct` of the values of this union.
   */
  def values(implicit values: Values[C]): values.Out = values(c)

  /**
   * Returns a `Coproduct` made of the key-value pairs of this union.
   */
  def fields(implicit fields: Fields[C]): fields.Out = fields(c)

  /**
   * Returns a `Map` whose keys and values are typed as the Lub of the keys
   * and values of this union.
   */
  def toMap[K, V](implicit toMap: ToMap.Aux[C, K, V]): Map[K, V] = toMap(c)

  /**
   * Maps a higher rank function across the values of this union.
   */
  def mapValues(f: Poly)(implicit mapValues: MapValues[f.type, C]): mapValues.Out = mapValues(c)

  /**
   * Returns a wrapped version of this union that provides `selectDynamic` access to fields.
   */
  def union: DynamicUnionOps[C] = DynamicUnionOps(c)
}

/**
 * Discriminated union wrapper providing `selectDynamic` access to fields.
 * 
 * @author Cody Allen
 */
final case class DynamicUnionOps[C <: Coproduct](c : C) extends Dynamic {
  import ops.union.Selector

  /**
   * Allows dynamic-style access to fields of the union whose keys are Symbols.
   */
  def selectDynamic(key: String)(implicit selector: Selector[C, Symbol @@ key.type]): selector.Out = selector(c)
}
