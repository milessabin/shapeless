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

/**
 * Record operations on `HList`'s with field-like elements.
 * 
 * @author Miles Sabin
 */
final class RecordOps[L <: HList](val l : L) extends AnyVal with Serializable {
  import shapeless.labelled._
  import ops.record._

  /**
   * Returns the value associated with the singleton typed key k. Only available if this record has a field with
   * with keyType equal to the singleton type K.
   */
  def get[K <: Singleton](k: K)(implicit selector : Selector[L, K]): selector.Out = selector(l)
  
  /**
   * Returns the value associated with the singleton typed key k. Only available if this record has a field with
   * with keyType equal to the singleton type K.
   *
   * Note that this can creates a bogus ambiguity with `HListOps#apply` as described in
   * https://issues.scala-lang.org/browse/SI-5142. If this method is accessible the conflict can be worked around by
   * using HListOps#at instead of `HListOps#apply`.
   */
  def apply[K <: Singleton](k: K)(implicit selector : Selector[L, K]): selector.Out = selector(l)

  /**
   * Returns the value associated with the singleton typed key k. Only available if this record has a field with
   * with keyType equal to the singleton type K.
   */
  def fieldAt[K <: Singleton](k: K)(implicit selector : Selector[L, K]): FieldType[K, selector.Out] = field[K](selector(l))

  /**
   * Updates or adds to this record a field with key k. The new field has a value of type V. Only available if this
   * record has a field with keyType equal to the singleton type K.
   */
  def updated[K <: Singleton, V](k: K, v: V)(implicit updater: Updater[L, FieldType[K, V]]) : updater.Out = updater(l, field[K](v))

  /**
   * Replaces the value of field k with a value of the same type. Only available if this record has a field with
   * keyType equal to the singleton type K and valueType equal to V.
   */
  def replace[K <: Singleton, V](k: K, v: V)
    (implicit ev: Selector.Aux[L, K, V], updater: Updater[L, FieldType[K, V]]): updater.Out = updater(l, field[K](v))
  
  /**
   * Updates a field having a value with type A by given function.
   */
  def updateWith[K <: Singleton, FSLOut, W](k: SingletonWithTypeAtField.Aux[L, K, FSLOut])(f: FSLOut => W)(implicit modifier: Modifier[L, K, FSLOut, W]): modifier.Out =
    modifier(l, f)

  /**
   * Remove the field associated with the singleton typed key k, returning both the corresponding value and the updated
   * record. Only available if this record has a field with keyType equal to the singleton type K.
   */
  def remove[K <: Singleton](k: K)(implicit remover: Remover[L, K]): remover.Out = remover(l)
  
  /**
   * Updates or adds to this record a field of type F.
   */
  def +[F](f: F)(implicit updater : Updater[L, F]): updater.Out = updater(l, f)

  /**
   * Remove the field associated with the singleton typed key k, returning the updated record. Only available if this
   * record has a field with keyType equal to the singleton type K.
   */
  def -[K <: Singleton, V, Out <: HList](k: K)(implicit remover : Remover.Aux[L, K, (V, Out)]): Out = remover(l)._2

  /**
   * Returns the union of this record and another record.
   */
  def merge[M <: HList](m: M)(implicit merger: Merger[L, M]): merger.Out = merger(l, m)

  /**
    * Returns the deep union of this record and another record.
    */
  def deepMerge[M <: HList](m: M)(implicit merger: DeepMerger[L, M]): merger.Out = merger(l, m)

  /**
    * Extracts super-record from sub-record according to depth subtype relation
    */
  def extract[E <: HList](implicit extractor: Extractor[L, E]): E = extractor(l)

  /**
    * Returns the union of this record and another record using the provided `f` to combine the values of fields which are present in both.
    *
    * The duplicated fields will be merged with `f`.
    */
  def mergeWith[M <: HList](m: M)(f: Poly)(implicit merger: MergeWith[L, M, f.type]): merger.Out = merger(l, m)

  /**
   * Rename the field associated with the singleton typed key oldKey. Only available if this
   * record has a field with keyType equal to the singleton type Old.
   */
  def renameField[Old <: Singleton, New <: Singleton](oldKey: Old, newKey: New)(implicit renamer: Renamer[L, Old, New]): renamer.Out = renamer(l)

  /**
   * Returns the keys of this record as an `HList` of singleton typed values.
   */
  def keys(implicit keys: Keys[L]): keys.Out = keys()

  /**
   * Returns a `HList` of the values of this record.
   */
  def values(implicit values: Values[L]): values.Out = values(l)

  /**
   * Returns a `HList` made of the key-value pairs of this record.
   */
  def fields(implicit fields: Fields[L]): fields.Out = fields(l)

  /**
   * Returns a `Map` whose keys and values are typed as the Lub of the keys
   * and values of this record.
   */
  def toMap[K, V](implicit toMap: ToMap.Aux[L, K, V]): Map[K, V] = toMap(l)

  /**
   * Maps a higher rank function across the values of this record.
   */
  def mapValues(f: Poly)(implicit mapValues: MapValues[f.type, L]): mapValues.Out = mapValues(l)

  /**
    * Align the keys on the order of HList of keys K
    */
  def alignByKeys[K <: HList](implicit alignByKeys: AlignByKeys[L, K]): alignByKeys.Out = alignByKeys(l)

  /**
   * Returns a wrapped version of this record that provides `selectDynamic` access to fields.
   */
  def record: DynamicRecordOps[L] = DynamicRecordOps(l)
}

/**
 * Record wrapper providing `selectDynamic` access to fields.
 * 
 * @author Cody Allen
 */
final case class DynamicRecordOps[L <: HList](l : L) extends Dynamic {
  import ops.record.Selector

  /**
   * Allows dynamic-style access to fields of the record whose keys are Symbols.
   */
  def selectDynamic[K <: String with Singleton](key: K)(implicit selector: Selector[L, K]): selector.Out = selector(l)
}

trait SingletonWithTypeAtField[L <: HList] {
  type K <: Singleton
  type Tpe
  val value: K
}
object SingletonWithTypeAtField {
  type Aux[L <: HList, K0 <: Singleton, Tpe0] = SingletonWithTypeAtField[L] { type K = K0; type Tpe = Tpe0 }

  implicit def fromKey[L <: HList, K0 <: Singleton, Out](k: K0)(implicit sel: ops.record.Selector.Aux[L, K0, Out]): SingletonWithTypeAtField.Aux[L, K0, Out] =
    new SingletonWithTypeAtField[L] {
      type K = K0
      type Tpe = Out
      val value: K0 = k
    }
}
