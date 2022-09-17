/*
 * Copyright (c) 2017 Fabio Labella
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

object inject {
  import ops.coproduct.{Inject, RuntimeInject}

  /**
   * @author Fabio Labella
   */
  implicit class InjectSyntax[T](val t: T) extends AnyVal {
    /**
     * Inject the receiver into a coproduct `C`.
     * Only available if the coproduct contains the type `T`.
     */
    def inject[C <: Coproduct](implicit inj: Inject[C, T]): C = inj(t)

    /**
     * Inject the receiver into a coproduct `C`, by trying to convert
     * it to each element of C.
     * Only available if the coproduct is not CNil.
     */
    def runtimeInject[C <: Coproduct](implicit rInj: RuntimeInject[C]): Option[C] =
      rInj(t)
  }
}
