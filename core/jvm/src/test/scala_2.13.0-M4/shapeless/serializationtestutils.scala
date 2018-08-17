/*
 * Copyright (c) 2018 Miles Sabin
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

import scala.collection.{ BuildFrom, Factory, IterableOps }
import scala.collection.generic.IsIterableLike
import scala.collection.mutable.Builder

object serializationtestutils {
  /**
   * A `Factory` for `List` implementing `Serializable`, unlike the one provided by the standard library.
   */
  implicit def listSerializableFactory[T]: Factory[T, List[T]] =
    new Factory[T, List[T]] with Serializable {
      def fromSpecific(it: IterableOnce[T]): List[T] = List.from(it)
      def newBuilder: Builder[T, List[T]] = List.newBuilder
    }

  /**
   * A `BuildFrom` for `List` implementing `Serializable`, unlike the one provided by the standard library.
   */
  implicit def listSerializableBuildFrom[From, T]: BuildFrom[From, T, List[T]] =
    new BuildFrom[From, T, List[T]] with Serializable {
      def fromSpecificIterable(from: From)(it: Iterable[T]): List[T] = List.from(it)
      def newBuilder(from: From): Builder[T, List[T]] = List.newBuilder
    }

  implicit def listSerializableIsIterableLike[T]: IsRegularIterable[List[T]] { type A = T } =
    new IsIterableLike[List[T]] with Serializable {
      type A = T
      val conversion: List[T] => IterableOps[T, Iterable, List[T]] = identity
    }
}
