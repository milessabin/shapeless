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

trait ShapelessVersionSpecifics {
  type BuildFrom[-F, -E, +T] = collection.BuildFrom[F, E, T]
  type Factory[-E, +T] = collection.Factory[E, T]
  type IsIterableLike[Repr] = collection.generic.IsIterableLike[Repr]
  type IterableLike[T, Repr] = collection.IterableOps[T, Iterable, Repr]
  type GenMap[K, +V] = Map[K, V]
}
