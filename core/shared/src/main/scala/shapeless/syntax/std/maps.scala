/*
 * Copyright (c) 2015 Miles Sabin
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

/**
 * Conversions between `Map` and `Records`.
 */
object maps {
  implicit def mapOps[K, V](m: Map[K, V]) = new MapOps[K, V](m)
}

final class MapOps[K, V](m: Map[K, V]) {
  /**
   * Extracts value from map to form Record L. Map must contain all the keys and the values
   * and of the correct types.
   * @param fm
   * @tparam R
   * @return Some[L] with values from Map or None if map does not match L
   */
  def toRecord[R <: HList](implicit fm: FromMap[R]): Option[R] = fm(m)
}
