/*
 * Copyright (c) 2013-18 Miles Sabin
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

package object shapeless extends ScalaVersionSpecifics {
  def unexpected : Nothing = sys.error("Unexpected invocation")

  // Basic definitions
  type Id[+T] = T
  type Const[C] = {
    type λ[T] = C
  }

  type ¬[T] = T => Nothing
  type ¬¬[T] = ¬[¬[T]]
  type ∧[T, U] = T with U
  type ∨[T, U] = ¬[¬[T] ∧ ¬[U]]

  // Type-lambda for context bound
  type |∨|[T, U] = {
    type λ[X] = ¬¬[X] <:< (T ∨ U)
  }

  // Type inequalities
  trait =:!=[A, B] extends Serializable

  implicit def neq[A, B] : A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A] : A =:!= A = unexpected
  implicit def neqAmbig2[A] : A =:!= A = unexpected

  @scala.annotation.implicitNotFound("${A} must not be a subtype of ${B}")
  trait <:!<[A, B] extends Serializable

  implicit def nsub[A, B] : A <:!< B = new <:!<[A, B] {}
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = unexpected
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = unexpected

  // Type-lambda for context bound
  type |¬|[T] = {
    type λ[U] = U <:!< T
  }

  // Quantifiers
  // TODO type ∃[P[_]] = P[_]
  // TODO type ∀[P[_]] = ¬[∃[({ type λ[X] = ¬[P[X]]})#λ]]

  /** `Optic` definitions */
  val optic = OpticDefns
  val lens = OpticDefns
  val prism = OpticDefns
  val ^ = Path

  /** `Nat` literals */
  val nat = Nat

  /** 'Fin' */
  val fin = Fin

  /** `Poly` definitions */
  val poly = PolyDefns
  import poly._

  /** Dependent nullary function type. */
  trait DepFn0 {
    type Out
    def apply(): Out
  }

  /** Dependent unary function type. */
  trait DepFn1[T] {
    type Out
    def apply(t: T): Out
  }

  /** Dependent binary function type. */
  trait DepFn2[T, U] {
    type Out
    def apply(t: T, u: U): Out
  }

  /** The SYB everything combinator */
  type Everything[F <: Poly, K <: Poly, T] = Case1[EverythingAux[F, K], T]

  def everything[T, R](f: Poly)(k: Poly)(t: T)(
    implicit cse: => Case1.Aux[EverythingAux[f.type, k.type], T, R]
  ): R = cse(t)

  /** The SYB everywhere combinator */
  type Everywhere[F <: Poly, T] = Case1[EverywhereAux[F], T]

  def everywhere[T, R](f: Poly)(t: T)(
    implicit cse: => Case1.Aux[EverywhereAux[f.type], T, R]
  ): R = cse(t)
}
