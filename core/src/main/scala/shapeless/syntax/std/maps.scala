/*
 * Copyright (c) 2011-13 Miles Sabin
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

package shapeless.syntax.std

import shapeless.HList
import shapeless.ops.maps.FromMap

import scala.collection.GenTraversable

/**
 * Conversions between `Map` and `Records`.
 *
 *
 */
object maps {
  implicit def traversableOps[T <: Map[_ <: Any, Any]](t: T) = new MapOps[T](t)
}

final class MapOps[T <: Map[_ <: Any, Any]](t: T) {
  /**
   * Extracts value from map to form Record L. Map must contain all the keys and the values
   * and of the correct types.
   * @param fl
   * @tparam L
   * @return Some[L] with values from Map or None if map does not match L
   */
  def toRecord[L <: HList](implicit fl: FromMap[L]): Option[ L] = fl(
    t.asInstanceOf[Map[Any, Any]])
}
