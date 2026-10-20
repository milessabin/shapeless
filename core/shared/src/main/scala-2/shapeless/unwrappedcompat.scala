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

trait TagUnwrappedInstances extends LowPriorityUnwrappedInstances {
  implicit def tagUnwrapped[T[UI, TT] <: tag.@@[UI, TT], UI, TT, UF](implicit
    chain: Strict[Unwrapped.Aux[UI, UF]]
  ): Unwrapped.Aux[T[UI, TT], UF] = chain.value.asInstanceOf[Unwrapped.Aux[T[UI, TT], UF]]
}

trait SelfUnwrappedScalaCompat { self: LowPriorityUnwrappedInstances =>
  implicit def selfUnwrapped[T]: Unwrapped.Aux[T,T] =
    theSelfUnwrapped.asInstanceOf[Unwrapped.Aux[T, T]]
}
