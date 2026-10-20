/*
 * Copyright (c) 2011-16 Miles Sabin
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

/** The value of a [[Lazy]]. */
trait LazyValue[+T] {
  val value: T
}

object LazyValue {
  def mkLazy[T](t: => T): Lazy[T] =
    new Lazy[T] {
      lazy val value: T = t
    }
}

trait LazyInstances {
  implicit def mkLazy[I](implicit i: => I): Lazy[I] = Lazy(i)
}
