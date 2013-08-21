/*
 * Copyright (c) 2011 Miles Sabin 
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
 * Record operations on `HList`'s with field-like elements.
 * 
 * @author Miles Sabin
 */
object record {
  import syntax.RecordOps

  implicit def recordOps[L <: HList](l : L) : RecordOps[L] = new RecordOps(l)

  /**
   * The type of fields with keys of singleton type `K` and value type `V`.
   */
  type FieldType[K, V] = V with KeyTag[K, V]
  trait KeyTag[K, V]

  /**
   * Yields a result encoding the supplied value with the singleton type `K' of its key.
   */
  def field[K] = new FieldBuilder[K]

  class FieldBuilder[K] {
    def apply[V](v : V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
}

/**
 * Field with values of type `V`.
 *
 * Record keys of this form should be objects which extend this trait. Keys may also be arbitrary singleton typed
 * values, however keys of this form enforce the type of their values.
 *
 * @author Miles Sabin
 */
trait FieldOf[V] {
  import record._

  def ->>(v: V): FieldType[this.type, V] = field[this.type](v)
}
