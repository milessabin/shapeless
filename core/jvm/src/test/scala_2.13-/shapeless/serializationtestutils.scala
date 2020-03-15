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

import scala.collection.GenTraversableLike
import scala.collection.generic.{ CanBuildFrom, IsTraversableLike }

object serializationtestutils {
 /**
  * A `CanBuildFrom` for `List` implementing `Serializable`, unlike the one provided by the standard library.
  */
 implicit def listSerializableCanBuildFrom[T]: CanBuildFrom[List[T], T, List[T]] =
   new CanBuildFrom[List[T], T, List[T]] with Serializable {
     def apply(from: List[T]) = from.genericBuilder[T]
     def apply() = List.newBuilder[T]
   }

  // To satisfy serialization of `ToSizedHList` we must provide a serializable `IsTraversableLike`
  //implicit val listIntSerializableIsTraversableLike: IsTraversableLike[List[Int]] { type A = Int } = null
  implicit def listSerializableIsIterableLike[T]: IsTraversableLike[List[T]] { type A = T } =
    new IsTraversableLike[List[T]] with Serializable {
      type A = T
      val conversion: List[T] => GenTraversableLike[T, List[T]] = identity
    }
}
