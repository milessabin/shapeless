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

package shapeless
package syntax

object typeable {
  implicit def typeableOps[T](t : T): TypeableOps[T] = new TypeableOps(t)
}

final class TypeableOps[T](val t : T) extends AnyVal with Serializable {
  /**
   * Cast the receiver to a value of type `U` if possible. This operation will be as precise wrt erasure as possible
   * given the in-scope `Typeable` instances available.
   */
  def cast[U](implicit castU : Typeable[U]) = castU.cast(t)

  /**
   * Cast the receiver to a value of subtype `U` of the receiver's static type if possible. This operation will be as
   * precise wrt erasure as possible given the in-scope `Typeable` instances available.
   */
  def narrowTo[U <: T](implicit castU: Typeable[U]) = castU.cast(t)
}
