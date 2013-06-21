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

object UnfoldExamples extends App {
  import shapeless._
  import Poly._
  import Nat._
  import HList._
  
  def typed[T](t : => T) {}
  
  trait Unfold[F <: Poly, E, S] {
    type Out <: HList
    def apply(s : S) : Out
  }
  
  object Unfold {
    implicit def unfold1[F <: Poly, E, S, Out0 <: HList]
      (implicit unfold : UnfoldAux[F, E, S, E, Out0]) : Unfold[F, E, S] =
        new Unfold[F, E, S] {
          type Out = Out0
          def apply(s : S) = unfold(s)
        }
    
    trait ApplyUnfold[E] {
      def apply[S, L <: HList](f : Poly)(s : S)
        (implicit unfold : UnfoldAux[f.type, E, S, E, L]) = unfold(s)
    }
    
    def unfold[E] = new ApplyUnfold[E] {} 
    def unfold[E](e : E) = new ApplyUnfold[E] {} 
  }
  
  trait UnfoldAux[F <: Poly, E, S, CoS, Out <: HList] {
    def apply(s : S) : Out
  }
  
  object UnfoldAux {
    implicit def unfold1[F <: Poly, S, CoS] : UnfoldAux[F, S, S, CoS, HNil] = new UnfoldAux[F, S, S, CoS, HNil] {
      def apply(s : S) = HNil
    }
    
    // The trick to prevent diverging implicits here is to have the term CoS (read: co-seed)
    // shrink at the same time as the term S (read: seed) grows. The only structure assumed
    // for the (co-)sequence of seeds is that implied by the cases of F.
    implicit def unfold2[F <: Poly, E, S, CoS, SS, OutH, OutT <: HList, PCoS, PCoSV]
      (implicit
        shrink : Pullback1Aux[F, PCoS, (PCoSV, CoS)],
        f : Pullback1Aux[F, S, (OutH, SS)],
        ut : UnfoldAux[F, E, SS, PCoS, OutT]) : UnfoldAux[F, E, S, CoS, OutH :: OutT] =
        new UnfoldAux[F, E, S, CoS, OutH :: OutT] {
          def apply(s : S) : OutH :: OutT = { 
            val (outH, sn) = f(s :: HNil)
            outH :: ut(sn)
          }
        }
  }
  
  import Unfold.unfold

  object unfoldMisc extends Poly1 {
    implicit def case0 = at[_0](_ => (23, _1))
    implicit def case1 = at[_1](_ => ("foo", _2))
    implicit def case2 = at[_2](_ => (true, _3))
    implicit def case3 = at[_3](_ => (1.0, _4))
  }

  val l1 = unfold(_3)(unfoldMisc)(_0)
  typed[Int :: String :: Boolean :: HNil](l1)
  println(l1)

  object unfoldFibs extends Poly1 {
    implicit def case0 = at[_0](_ => (_0, _1))
    implicit def case1 = at[_1](_ => (_1, _2))
    implicit def caseN[N <: Nat, FN <: Nat, FSN <: Nat, FSSN <: Nat]
      (implicit
        fn : Pullback1[N, (FN, Succ[N])],
        fsn : Pullback1[Succ[N], (FSN, Succ[Succ[N]])],
        sum : SumAux[FN, FSN, FSSN],
        fssn : FSSN) =
      at[Succ[Succ[N]]](_ => (fssn, Succ[Succ[Succ[N]]]))
  }

  object toInt extends Poly1 {
    implicit def default[N <: Nat](implicit toInt : ToInt[N]) = at[N](_ => toInt())
  }
  
  val l2 = unfold(_6)(unfoldFibs)(_0)
  typed[_0 :: _1 :: _1 :: _2 :: _3 :: _5 :: HNil](l2)
  println(l2 map toInt)
}
