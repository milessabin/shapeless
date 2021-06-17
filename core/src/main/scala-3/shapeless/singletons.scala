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

trait WitnessScalaCompat {
  def selectDynamic(tpeSelector: String): Any = ???
}

trait NatWithScalaCompat {

  implicit def apply[TC[_ <: Nat]](i: Any): NatWith[TC] = ???

  implicit def apply2[B, T <: B, TC[_ <: B, _ <: Nat]](i: Int): NatWith[({ type λ[t <: Nat] = TC[T, t] })#λ] = ???
}

trait WidenScalaCompat {

  implicit def apply1[TC[_], T](t: T): WitnessWith.Lt[TC, T] = ???

  implicit def materialize[T, Out >: T]: Widen.Aux[T, Out] = ???
}
