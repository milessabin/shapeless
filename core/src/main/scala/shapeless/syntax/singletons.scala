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

trait SingletonOps {
  import labelled._

  type T

  /**
   * Returns a value of the singleton type of this value.
   */
  val value: T

  /**
   * Narrows this value to its singleton type.
   */
  def narrow: T {} = value

  /**
   * Returns the provided value tagged with the singleton type of this value as its key in a record-like structure.
   */
  def ->>[V](v: V): FieldType[T, V] = field[T](v)
}

object SingletonOps {
  type Aux[A] = SingletonOps { type T = A }

  def instance[A <: Singleton](w: A): Aux[A] =
    new SingletonOps {
      type T = A
      val value = w
    }
}

object singleton {
  implicit def mkSingletonOps[T <: Singleton](t: T): SingletonOps.Aux[t.type] =
    SingletonOps.instance[t.type](t)
}
