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

import TypeOperators._

/**
 * Type-specific case of a polymorphic value.
 * 
 * @author Miles Sabin
 */
abstract class Case0Aux[-P, T] {
  val value : T
  def apply() = value
}

object Case0Aux {
  def apply[P, T](v : T) = new Case0Aux[P, T] {
    val value = v
  }
  
  implicit def inst[P, T](c : Case0Aux[P, T]) : T = c.value
}

/**
 * Type-specific case of a polymorphic unary function.
 * 
 * @author Miles Sabin
 */
abstract class Case1Aux[-P, T] {
  type R
  val value : T => R
  def apply(t : T) = value(t)
}

object Case1Aux {
  def apply[P, T, R0](v : T => R0) = new Case1Aux[P, T] {
    type R = R0
    val value = v
  }
  
  implicit def inst[P, T, R0](c : Case1Aux[P, T] { type R = R0 }) : T => R0 = c.value
}

/**
 * Type-specific case of a polymorphic binary function.
 * 
 * @author Miles Sabin
 */
abstract class Case2Aux[-P, T, U] {
  type R
  val value : (T, U) => R
  def apply(t : T, u : U) = value(t, u)
}

object Case2Aux {
  def apply[P, T, U, R0](v : (T, U) => R0) = new Case2Aux[P, T, U] {
    type R = R0
    val value = v
  }
  
  implicit def inst[P, T, U, R0](c : Case2Aux[P, T, U] { type R = R0 }) : (T, U) => R0 = c.value
}

/**
 * Base trait for polymorphic values.
 * 
 * @author Miles Sabin
 */
trait Poly {
  /** The type of the case representing this polymorphic value at type `T`. */
  type Case0[T] = Case0Aux[this.type, T]
  
  /** The type of the case representing this polymorphic unary function at argument type `T`. */
  type Case1[T] = Case1Aux[this.type, T]
  
  /** The type of the case representing this polymorphic binary function at argument types `T` and `U`. */
  type Case2[T, U] = Case2Aux[this.type, T, U]

  def apply[T](implicit c : Case0[T]) : T = c()
  def apply[T](t : T)(implicit c : Case1[T]) : c.R = c(t)
  def apply[T, U](t : T, u : U)(implicit c : Case2[T, U]) : c.R = c(t, u)

  /** The type of a case of this polymorphic function of the form `T => T` */
  type Hom[T] = Case1[T] { type R = T }

  /** The type of a case of this polymorphic function of the form `T => R` */
  type Pullback1[T, R0] = Case1[T] { type R = R0 }

  /** The type of a case of this polymorphic function of the form `(T, U) => R` */
  type Pullback2[T, U, R0] = Case2[T, U] { type R = R0 }
}

trait Poly0 extends Poly {
  /** Creates an instance of the case representing this polymorphic value at type `T`. */
  def at[T](v : T) = new Case0[T] { val value = v }
}

trait Poly1 extends Poly {
  /** Creates an instance of the case representing this polymorphic unary function at argument type `T`. */
  def at[T] = new Case1Builder[T]
  class Case1Builder[T] {
    def apply[R0](f : T => R0) = new Case1[T] { type R = R0 ; val value = f }
  }
}

trait Poly2 extends Poly {
  /** Creates an instance of the case representing this polymorphic binary function at argument types `T` and `U`. */
  def at[T, U] = new Case2Builder[T, U]
  class Case2Builder[T, U] {
    def apply[R0](f : (T, U) => R0) = new Case2[T, U] { type R = R0 ; val value = f }
  }
}

trait Pullback1[R0] extends Poly {
  /** Creates an instance of the case representing this polymorphic unary function at argument type `T`. */
  def at[T](f : T => R0) = new Case1[T] { type R = R0 ; val value = f }
}

trait Pullback2[R0] extends Poly {
  /** Creates an instance of the case representing this polymorphic binary function at argument types `T` and `U`. */
  def at[T, U](f : (T, U) => R0) = new Case2[T, U] { type R = R0 ; val value = f }
}

/**
 * Provides implicit conversions from polymorphic function values to monomorphic function values, eg. for use as
 * arguments to ordinary higher order functions.
 *  
 * @author Miles Sabin
 */
object Poly {
  implicit def inst0[P <: Poly, T](p : P)(implicit c : p.Case0[T]) : T = c.value
  implicit def inst1[P <: Poly, T](p : P)(implicit c : p.Case1[T]) : T => c.R = c.value
  implicit def inst2[P <: Poly, T, U](p : P)(implicit c : p.Case2[T, U]) : (T, U) => c.R = c.value

  type HomAux[-P, T] = Case1Aux[P, T] { type R = T }
  type Pullback1Aux[-P, T, R0] = Case1Aux[P, T] { type R = R0 }
  type Pullback2Aux[-P, T, U, R0] = Case2Aux[P, T, U] { type R = R0 }
}

/**
 * Base trait for natural transformations.
 * 
 * @author Miles Sabin
 */
trait ~>[F[_], G[_]] extends Poly1 {
  def default[T](f : F[T]) : G[T]
  def apply[T](f : F[T]) = default(f)
  implicit def caseUniv[T] = at[F[T]](default[T] _)
}

object ~> {
  implicit def inst1[G[_], T](p : Id ~> G) : T => G[T] = p.caseUniv[T].value
  implicit def inst2[F[_], G[_], T](p : F ~> G) : F[T] => G[T] = p.caseUniv[T].value
}

trait ~>>[F[_], R] extends Pullback1[R] {
  def default[T](f : F[T]) : R
  def apply[T](f : F[T]) = default(f)
  implicit def caseUniv[T] = at[F[T]](default[T] _)
}

/**
 * Type class witnessing the existence of a natural transformation between `K[_]` and `V[_]`
 * 
 * @author Miles Sabin
 */
class ~?>[K[_], V[_]] {
  class λ[K, V]
}

object ~?> {
  implicit def rel[K[_], V[_]] : K ~?> V = new (K ~?> V)
  
  implicit def witness[K[_], V[_], T](implicit rel : K ~?> V) : rel.λ[K[T], V[T]] = new rel.λ[K[T], V[T]] 
}

/** Polymorphic identity function. */
object identity extends (Id ~> Id) {
  def default[T](t : T) = t
}

/** Polymorphic singleton function. */
object singleton extends (Id ~> Set) {
  def default[T](t : T) = Set(t)
}

/** Polymorphic function selecting an arbitrary element from a non-empty `Set`. */
object choose extends (Set ~> Option) {
  def default[T](s : Set[T]) = s.headOption 
}

/** Polymorphic function creating singleton `List`s. */
object list extends (Id ~> List) {
  def default[T](t : T) = List(t)
}

/** Polymorphic function returning the head of a `List`. */
object headOption extends (List ~> Option) {
  def default[T](l : List[T]) = l.headOption
}

/** Polymorphic function testing whether or not an `Option` is defined. */
object isDefined extends (Option ~>> Boolean) {
  def default[T](o : Option[T]) = o.isDefined
}

/** Polymorphic function which opens an `Option`. */
object get extends (Option ~> Id) {
  def default[T](o : Option[T]) = o.get
}

/** Polymorphic function which injects a value into an `Option`. */
object option extends (Id ~> Option) {
  def default[T](t : T) = Option(t)
}

/** Polymorphic addition with type specific cases. */
object plus extends Poly2 {
  implicit val caseInt = at[Int, Int](_ + _)
  implicit val caseDouble = at[Double, Double](_ + _)
  implicit val caseString = at[String, String](_ + _)
  implicit def caseList[T] = at[List[T], List[T]](_ ::: _)
}

/** Polymorphic zero with type specific cases. */
object zero extends Poly0 {
  implicit val zeroInt = at[Int](0) 
  implicit val zeroDouble = at[Double](0.0) 
  implicit val zeroString = at[String]("") 
  implicit def zeroList[T] = at[List[T]](Nil)
}
