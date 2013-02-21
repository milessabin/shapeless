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

package shapeless

object TypeOperators {
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
  trait =:!=[A, B] 

  implicit def neq[A, B] : A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A] : A =:!= A = unexpected
  implicit def neqAmbig2[A] : A =:!= A = unexpected
  
  trait <:!<[A, B]

  implicit def nsub[A, B] : A <:!< B = new <:!<[A, B] {}
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = unexpected
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = unexpected

  // Type-lambda for context bound
  type |¬|[T] = {
    type λ[U] = U <:!< T
  }

  // Quantifiers
  type ∃[P[_]] = P[T] forSome { type T }
  type ∀[P[_]] = ¬[∃[({ type λ[X] = ¬[P[X]]})#λ]]

  // Tags
  trait Tagged[U]
  type @@[T, U] = T with Tagged[U]

  class Tagger[U] {
    def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
  }
  
  def tag[U] = new Tagger[U]

  // Newtype
  
  /**
   * New type with `Repr` as representation type and operations provided by `Ops`.
   * 
   * Values of the newtype will not add any additional boxing beyond what's required for
   * values of the representation type to conform to Any. In practice this means that value
   * types will receive their standard Scala AnyVal boxing and reference types will be unboxed.
   */
  type Newtype[Repr, Ops] = Any @@ NewtypeTag[Repr, Ops]
  trait NewtypeTag[Repr, Ops]
  
  /**
   * Creates a value of the newtype given a value of its representation type. 
   */
  def newtype[Repr, Ops](r : Repr) : Newtype[Repr, Ops] = r.asInstanceOf[Newtype[Repr, Ops]]
  
  /**
   * Implicit conversion of newtype to `Ops` type for the selection of `Ops` newtype operations.
   * 
   * The implicit conversion `Repr => Ops` would typically be provided by publishing the companion
   * object of the `Ops` type as an implicit value.
   */
  implicit def newtypeOps[Repr, Ops](t : Newtype[Repr, Ops])(implicit mkOps : Repr => Ops) : Ops = t.asInstanceOf[Repr] 
}
