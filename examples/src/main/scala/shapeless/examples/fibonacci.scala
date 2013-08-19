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
 * Type-level encoding of the Fibonacci numbers.
 * 
 * @author Miles Sabin
 */
object FibonacciExamples {
  import shapeless._
  import nat._
  import ops.nat._
  import ops.hlist.Reverse

  def typed[T](t : => T) {}
  
  // Compute the ith fibonacci number

  class Fibonacci[I <: Nat, N <: Nat]
  
  object Fibonacci {
    def apply(i: Nat, j: Nat) = new Fibonacci[i.N, j.N]

    implicit val fib0 = Fibonacci(0, 0)
    implicit val fib1 = Fibonacci(1, 1)
  
    implicit def fibN[I <: Nat, L <: Nat, M <: Nat]
      (implicit l : Fibonacci[I, L], m : Fibonacci[Succ[I], M], sum : Sum[L, M]) =
        new Fibonacci[Succ[Succ[I]], sum.Out]
  }
  
  def fibonacci[N <: Nat](i : Nat)(implicit fib : Fibonacci[i.N, N], wn: Witness.Aux[N]): N = wn.value

  val f0 = fibonacci(0)
  typed[_0](f0)
  
  val f1 = fibonacci(1)
  typed[_1](f1)

  val f2 = fibonacci(2)
  typed[_1](f2)

  val f3 = fibonacci(3)
  typed[_2](f3)

  val f4 = fibonacci(4)
  typed[_3](f4)
  
  val f5 = fibonacci(5)
  typed[_5](f5)
  
  val f6 = fibonacci(6)
  typed[_8](f6)

  val f7 = fibonacci(7)
  typed[_13](f7)

  // Compute an HList of the first N fibonacci numbers

  trait Fibs[N <: Nat, Out <: HList] {
    def apply() : Out
  }

  object Fibs {
    implicit def fibs0 = new Fibs[_0, HNil] {
      def apply() = HNil
    }
    
    implicit def fibsN[N <: Nat, H <: Nat, T <: HList]
      (implicit fib : Fibonacci[N, H], h : Witness.Aux[H], fibs : Fibs[N, T]) =
        new Fibs[Succ[N], H :: T] {
      def apply() = h.value :: fibs()
    }
  }
  
  def fibs[L <: HList](n : Nat)
    (implicit fibs : Fibs[n.N, L], reverse : Reverse[L]) = fibs().reverse

  val l0 = fibs(0)
  typed[HNil](l0)
  
  val l1 = fibs(1)
  typed[_0 :: HNil](l1)

  val l2 = fibs(2)
  typed[_0 :: _1 :: HNil](l2)

  val l3 = fibs(3)
  typed[_0 :: _1 :: _1 :: HNil](l3)

  val l4 = fibs(4)
  typed[_0 :: _1 :: _1 :: _2 :: HNil](l4)

  val l5 = fibs(5)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: HNil](l5)

  val l6 = fibs(6)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: _5 :: HNil](l6)

  val l7 = fibs(7)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: _5 :: _8 :: HNil](l7)

  val l8 = fibs(8)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: _5 :: _8 :: _13 :: HNil](l8)
}
