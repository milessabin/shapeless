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

import scala.annotation.implicitNotFound

import shapeless._
import labelled._

object maps {
  /**
   * Type class supporting type safe conversion of Map to Records.
   */
  @implicitNotFound("Implicit not found: shapeless.Ops.FromMap[${R}]. Maps can only be converted to appropriate Record types.")
  trait FromMap[R <: HList] extends Serializable {
    def apply[K, V](m: Map[K, V]): Option[R]
  }

  /**
   * `FromMap` type class instances.
   */
  object FromMap {
    def apply[R <: HList](implicit fm: FromMap[R]) = fm

    implicit def hnilFromMap[T]: FromMap[HNil] =
      new FromMap[HNil] {
        def apply[K, V](m: Map[K, V]): Option[HNil] = Some(HNil)
      }


    implicit def hlistFromMap[K0, V0, T <: HList]
      (implicit wk: Witness.Aux[K0], tv: Typeable[V0], fmt: FromMap[T]): FromMap[FieldType[K0, V0] :: T] =
        new FromMap[FieldType[K0, V0] :: T] {
          def apply[K, V](m: Map[K, V]): Option[FieldType[K0, V0] :: T] = {
            for {
              value <- m.get(wk.value.asInstanceOf[K])
              typed <- tv.cast(value)
              rest <- fmt(m)
            } yield field[K0](typed) :: rest
          }
        }
  }
}
