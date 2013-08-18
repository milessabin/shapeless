/*
 * Copyright (c) 2013 Miles Sabin 
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

object BooleanInduction extends App {
  import shapeless._
  import syntax.singleton._

  // Some preliminaries ...

  val (wTrue, wFalse) = (true.witness, false.witness)
  type True = wTrue.T
  type False = wFalse.T

  trait If[C <: Boolean, A, B] { type T ; def apply(a: A, b: B): T }
  object If {
    implicit def ifTrue[A, B]  = new If[True, A, B]  { type T = A ; def apply(a: A, b: B) = a }
    implicit def ifFalse[A, B] = new If[False, A, B] { type T = B ; def apply(a: A, b: B) = b }
  }

  // Scala translation of:
  //
  // bool-induction : (P : Bool->Type) (t : P true) (f: P false) (x : Bool) -> P x
  // bool-induction P pt pf true = pt
  // bool-induction P pt pf false = pf

  def boolInduction[P <: { type Case[_ <: Boolean] <: { type T }}, PT, PF]
    (p: P)(t: PT)(f: PF)(x: Witness.Lt[Boolean])
      (implicit pt: p.Case[True] { type T = PT }, pf: p.Case[False] { type T = PF }, sel: If[x.T, PT, PF]): sel.T = sel(t, f)

  // In use ...

  object si {
    trait Case[B <: Boolean] { type T }
    implicit val sit  = new Case[True]  { type T = String }
    implicit val sif  = new Case[False] { type T = Int }
  }

  val bt: String = boolInduction(si)("foo")(23)(true) 
  val bf: Int    = boolInduction(si)("foo")(23)(false)
}

