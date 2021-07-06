/*
 * Copyright (c) 2013-16 Miles Sabin
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

/**
 * Provides the widen type of a singleton type.
 *
 * Type member `Out` of an implicitly available `Witness[T]` instance is the widen type
 * of `T`, and the `apply` method explicitly converts a `T` to an `Out`.
 *
 * E.g. if `T` is ``Witness.`2`.T``, `Out` is `Int`.
 *
 * It somehow complements `Witness`, providing the corresponding non-witnessed standard type, if any.
 *
 * Example of use,
 * {{
 *   val w = Widen[Witness.`2`.T]
 *   // w.Out is Int
 *   // w(2) is typed as Int
 * }}
 *
 * @author Alexandre Archambault
 */
trait Widen[T] extends DepFn1[T] { type Out >: T }

object Widen extends WidenScalaCompat {
  def apply[T](implicit widen: Widen[T]): Aux[T, widen.Out] = widen

  type Aux[T, Out0 >: T] = Widen[T] { type Out = Out0 }

  def instance[T, Out0 >: T](f: T => Out0): Aux[T, Out0] =
    new Widen[T] {
      type Out = Out0
      def apply(t: T) = f(t)
    }
}
