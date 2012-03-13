/*
 * Copyright (c) 2011 Miles Sabin 
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
  import Nat._
  import HList._

  def typed[T](t : => T) {}
  
  // Compute the ith fibonacci number
  
  trait Fibonacci[I <: Nat] {
    type N <: Nat
  }
  
  object Fibonacci {
    implicit def fibonacci[I <: Nat, N0 <: Nat](implicit fib : FibonacciAux[I, N0]) = new Fibonacci[I] {
      type N = N0
    }
    
    def fibonacci[I <: Nat, N <: Nat](i : I)(implicit fib : FibonacciAux[I, N], n : N) = n
  }
  
  trait FibonacciAux[I <: Nat, N <: Nat]
  
  object FibonacciAux {
    implicit def fib0 = new FibonacciAux[_0, _0] {}
    implicit def fib1 = new FibonacciAux[_1, _1] {}
  
    implicit def fibN[I <: Nat, L <: Nat, M <: Nat, N <: Nat]
      (implicit l : FibonacciAux[I, L], m : FibonacciAux[Succ[I], M], sum : SumAux[L, M, N]) =
        new FibonacciAux[Succ[Succ[I]], N] {}
  }
  
  import Fibonacci._
  
  val f0 = fibonacci(_0)
  typed[_0](f0)
  
  val f1 = fibonacci(_1)
  typed[_1](f1)

  val f2 = fibonacci(_2)
  typed[_1](f2)

  val f3 = fibonacci(_3)
  typed[_2](f3)

  val f4 = fibonacci(_4)
  typed[_3](f4)
  
  val f5 = fibonacci(_5)
  typed[_5](f5)
  
  val f6 = fibonacci(_6)
  typed[_8](f6)

  val f7 = fibonacci(_7)
  typed[_13](f7)

  // Compute an HList of the first N fibonacci numbers
  trait Fibs[N <: Nat] {
    type Out <: HList
    def apply() : Out
  }
  
  object Fibs {
    implicit def fibs[N <: Nat, L0 <: HList](implicit fibs : FibsAux[N, L0]) = new Fibs[N] {
      type Out = L0
      def apply() = fibs()
    }
    
    def fibs[N <: Nat, L <: HList](n : N)
      (implicit fibs : FibsAux[N, L], reverse : Reverse[L]) = fibs().reverse
  }

  trait FibsAux[N <: Nat, Out <: HList] {
    def apply() : Out
  }

  object FibsAux {
    implicit def fibs0 = new FibsAux[_0, HNil] {
      def apply() = HNil
    }
    
    implicit def fibsN[N <: Nat, H <: Nat, T <: HList]
      (implicit fib : FibonacciAux[N, H], h : H, fibs : FibsAux[N, T]) =
        new FibsAux[Succ[N], H :: T] {
      def apply() = h :: fibs()
    }
  }
  
  import Fibs._
  
  val l0 = fibs(_0)
  typed[HNil](l0)
  
  val l1 = fibs(_1)
  typed[_0 :: HNil](l1)

  val l2 = fibs(_2)
  typed[_0 :: _1 :: HNil](l2)

  val l3 = fibs(_3)
  typed[_0 :: _1 :: _1 :: HNil](l3)

  val l4 = fibs(_4)
  typed[_0 :: _1 :: _1 :: _2 :: HNil](l4)

  val l5 = fibs(_5)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: HNil](l5)

  val l6 = fibs(_6)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: _5 :: HNil](l6)

  val l7 = fibs(_7)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: _5 :: _8 :: HNil](l7)

  val l8 = fibs(_8)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: _5 :: _8 :: _13 :: HNil](l8)
}