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

trait RefuteScalaCompat {

  trait Impl[A]
  object Impl {
    /** This results in  ambigous implicits if there is implicit evidence of `T` */
    implicit def amb1[T](implicit ev: T): Impl[T] = null
    implicit def amb2[T]: Impl[T] = null
  }

  /** This always declares an instance of `Refute`
    *
    * This instance will only be found when there is no evidence of `T`
    * */
  implicit def refute[T](implicit dummy: Impl[T]): Refute[T] = new Refute[T] {}
}
