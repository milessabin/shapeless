/*
 * Copyright (c) 2011-13 Miles Sabin 
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

package shapeless.examples

/**
 * Type-level encoding of factorial function.
 * 
 * @author George Leontiev
 */

object FactorialExamples {
  import shapeless._
  import nat._

  def typed[T](t : => T) {}

  trait Factorial[I <: Nat] {
    type N <: Nat
  }

  object Factorial {
    implicit def factorial1[I <: Nat, N0 <: Nat](implicit fact : FactorialAux[I, N0]) = new Factorial[I] {
      type N = N0
    }

    def factorial[N <: Nat](i : Nat)(implicit fact : FactorialAux[i.N, N], wn : WitnessAux[N]) = wn.value
  }

  trait FactorialAux[I <: Nat, N <: Nat]

  object FactorialAux {
    implicit def fact0 = new FactorialAux[_0, _1] {}
    implicit def factN[N <: Nat, F <: Nat, F1 <: Nat]
      (implicit f : FactorialAux[N, F1], t : ProdAux[Succ[N], F1, F]) =
        new FactorialAux[Succ[N], F] {}
  }

  import Factorial._

  val f0 = factorial(0)
  typed[_1](f0)

  val f1 = factorial(1)
  typed[_1](f1)

  val f2 = factorial(2)
  typed[_2](f2)

  val f3 = factorial(3)
  typed[_6](f3)
}
