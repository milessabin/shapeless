/*
 * Copyright (c) 2012 Miles Sabin 
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

object UnfoldExamples {
  import shapeless._
  import Poly._
  import Nat._
  import HList._
  
  def typed[T](t : => T) {}
  
  trait Unfold[F <: Poly1, E, S] {
    type Out <: HList
    def apply(s : S) : Out
  }
  
  object Unfold {
    implicit def unfold1[F <: Poly1, E, S, Out0 <: HList]
      (implicit unfold : UnfoldAux[F, E, S, Out0]) : Unfold[F, E, S] =
        new Unfold[F, E, S] {
          type Out = Out0
          def apply(s : S) = unfold(s)
        }
    
    trait ApplyUnfold[E] {
      def apply[F <: Poly1, S, L <: HList](f : F)(s : S)
        (implicit unfold : UnfoldAux[F, E, S, L]) = unfold(s)
    }
    
    def unfold[E] = new ApplyUnfold[E] {} 
  }
  
  trait UnfoldAux[F <: Poly1, E, S, Out <: HList] {
    def apply(s : S) : Out
  }
  
  object UnfoldAux {
    implicit def unfold1[F <: Poly1, S] : UnfoldAux[F, S, S, HNil] = new UnfoldAux[F, S, S, HNil] {
        def apply(s : S) = HNil
      }
    implicit def unfold2[F <: Poly1, E, S, Sn, OutH, OutT <: HList]
      (implicit f : Pullback1Aux[F, S, (OutH, Sn)], ut : UnfoldAux[F, E, Sn, OutT]) : UnfoldAux[F, E, S, OutH :: OutT] =
        new UnfoldAux[F, E, S, OutH :: OutT] {
          def apply(s : S) : OutH :: OutT = { 
            val (outH, sn) = f(s)
            outH :: ut(sn)
          }
        }
  }
  
  import Unfold.unfold

  class A; class B; class C; class D; class E
  
  object unfoldFn1 extends Poly1 {
    implicit def case0 = at[A](_ => (23, new B))
    implicit def case1 = at[B](_ => ("foo", new C))
    implicit def case2 = at[C](_ => (true, new D))
    implicit def case3 = at[D](_ => (1.0, new E))
  }

  val l1 = unfold[D](unfoldFn1)(new A {})
  typed[Int :: String :: Boolean :: HNil](l1)
}
