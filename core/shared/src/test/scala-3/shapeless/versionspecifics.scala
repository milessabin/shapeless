/*
 * Copyright (c) 2011-14 Miles Sabin
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

/** Scala 3 spellings of types which tests compare against inferred types, following Scala 3 type inference. */
object TestVersionSpecifics {
  /** The least upper bound of case classes extending `T`, as computed by `Lub` (and inferred by Scala 3). */
  type CaseClassLub[T] = T

  /** The least upper bound of unrelated types `A` and `B`. */
  type Lub2[A, B] = A | B

  /** The least upper bound of unrelated types `A`, `B` and `C`. */
  type Lub3[A, B, C] = A | B | C

  /** Implicitly available `ClassTag[Nothing]`, which Scala 3 doesn't provide. */
  object NothingClassTag {
    implicit val classTag: scala.reflect.ClassTag[Nothing] = shapeless.ops.ArrayOfNothing.classTag
  }
}
