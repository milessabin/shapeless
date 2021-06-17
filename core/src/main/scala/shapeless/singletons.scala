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

import scala.language.dynamics

/** Provides the value corresponding to a singleton type.
 *
 * See SIP-23 for a related proposed language change.
 */
trait Witness extends Serializable {
  type T
  val value: T {}
}

object Witness extends Dynamic with WitnessInstances with WitnessScalaCompat {
  type Aux[T0] = Witness { type T = T0 }
  type Lt[Lub] = Witness { type T <: Lub }

  def mkWitness[A](v: A): Aux[A] = new Witness {
    type T = A
    val value = v
  }

  implicit val witness0: Witness.Aux[_0] =
    mkWitness(Nat._0)

  implicit def witnessN[P <: Nat]: Witness.Aux[Succ[P]] =
    mkWitness(Succ[P]())
}

trait WitnessWith[TC[_]] extends Witness {
  val instance: TC[T]
}

object WitnessWith extends WitnessWithInstances {
  type Aux[TC[_], T0] = WitnessWith[TC] { type T = T0 }
  type Lt[TC[_], Lub] = WitnessWith[TC] { type T <: Lub }
}

trait NatWith[TC[_ <: Nat]] {
  type N <: Nat
  val instance: TC[N]
}

object NatWith extends NatWithScalaCompat {
  type Aux[TC[_ <: Nat], N0 <: Nat] = NatWith[TC] { type N = N0 }

  def depInstance[TC[_ <: Nat] <: AnyRef, N0 <: Nat](tc: TC[N0]): Aux[TC, N0] { val instance: tc.type } =
    new NatWith[TC] {
      type N = N0
      val instance: tc.type = tc
    }

  def instance[TC[_ <: Nat], N0 <: Nat](tc: TC[N0]): Aux[TC, N0] =
    new NatWith[TC] {
      type N = N0
      val instance: TC[N] = tc
    }
}

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
