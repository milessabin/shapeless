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
package ops


object fin {
  /**
   * Type class supporting conversion of type-level Nats to a Fin, only available if M < N
   */
  trait FromNat[M <: Nat, N <: Succ[_]] extends DepFn0 { type Out <: Fin[N] }

  object FromNat {
    def apply[M <: Nat, N <: Succ[_]]
      (implicit fromNat: FromNat[M, N]): Aux[M, N, fromNat.Out] = fromNat

    type Aux[M <: Nat, N <: Succ[_], Out0 <: Fin[N]] = FromNat[M, N] { type Out = Out0 }

    implicit def finZeroFromNat[N <: Succ[_]]: Aux[_0, N, FinZero[N]] = new FromNat[_0, N] {
      type Out = FinZero[N]

      def apply(): Out = FinZero[N]()
    }

    implicit def finSuccFromNat[M <: Nat, N <: Succ[_]]
      (implicit fromNat: FromNat[M, N]): Aux[Succ[M], Succ[N], FinSucc[N, fromNat.Out]] =
        new FromNat[Succ[M], Succ[N]] {
          type Out = FinSucc[N, fromNat.Out]

          def apply(): Out = FinSucc[N, fromNat.Out]()
        }
  }


  /**
   * Type class supporting conversion of a Fin to a Nat.
   */
  trait ToNat[F <: Fin[_]] extends DepFn0 {
    type Out <: Nat
  }

  object ToNat {
    def apply[F <: Fin[_]](implicit toNat: ToNat[F]): Aux[F, toNat.Out] = toNat

    type Aux[F <: Fin[_], Out0 <: Nat] = ToNat[F] { type Out = Out0 }

    implicit def finZeroToNat[N <: Succ[_]]: Aux[FinZero[N], Nat._0] = new ToNat[FinZero[N]] {
      type Out = Nat._0

      def apply() = Nat._0
    }

    implicit def finSuccToNat[N <: Succ[_], F <: Fin[N], M <: Nat](
      implicit nat: Aux[F, M]
    ): Aux[FinSucc[N, F], Succ[M]] = new ToNat[FinSucc[N, F]] {
      type Out = Succ[M]

      def apply() = Succ()
    }
  }
}
