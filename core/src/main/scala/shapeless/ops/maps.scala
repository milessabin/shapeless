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

package shapeless.ops

import shapeless._
import shapeless.labelled.FieldType

object maps {

  /**
   * Type class supporting type safe conversion of Map to Records.
   *
   *
   */
  trait FromMap[Out <: HList] extends Serializable {
    def apply(l: Map[Any, Any]): Option[Out]
  }

  /**
   * `FromMap` type class instances.
   *
   *
   */
  object FromMap {

    def apply[Out <: HList](implicit from: FromMap[Out]) = from

    implicit def hnilFromMap[T] = new FromMap[HNil] {

      override def apply(l: Map[Any, Any]) = Some(HNil)
    }

    import labelled._

    implicit def hlistFromMap[K, V, OutT <: HList](implicit flt: FromMap[OutT], oc: Typeable[V],
                                                   wit: Witness.Aux[K]) = new
        FromMap[FieldType[K, V] :: OutT] {

      import labelled._

      def apply(mm: Map[Any, Any]): Option[FieldType[K, V] :: OutT] = {

        val key = wit.value

        for {value <- mm.get(key)
             typed <- oc.cast(value)
             rest <- flt(mm)} yield field[K](typed) :: rest
      }
    }
  }
}