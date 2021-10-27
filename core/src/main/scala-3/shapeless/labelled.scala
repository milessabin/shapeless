/*
 * Copyright (c) 2014-16 Miles Sabin
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

import scala.deriving._
import scala.compiletime._

object labelled {
  
  /** The type of fields with keys of singleton type `K` and value type `V`. */
  opaque type FieldType[K, +V] <: V = V

  type ->>[K, +V] = FieldType[K, V]

  /** Yields a result encoding the supplied value with the singleton type `K` of its key. */
  def field[K]: FieldBuilder[K] = new FieldBuilder(true)
  class FieldBuilder[K](private val dummy: Boolean) extends AnyVal {
    def apply[V](v: V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
}

trait LabellingScalaCompat {

  inline given [T](using m: Mirror.Of[T]): Labelling.Aux[T, HList.TupleToHList[m.MirroredElemLabels]] = {
    val tuple = constValueTuple[m.MirroredElemLabels]
    val res = HList.tupleToHList(tuple)

    new Labelling[T] {
      override type Out = HList.TupleToHList[m.MirroredElemLabels]
      override def apply(): Out = res
    }
  }
}
