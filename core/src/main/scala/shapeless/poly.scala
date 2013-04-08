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

package shapeless

import TypeOperators._

/**
 * Type-specific case of a polymorphic function.
 * 
 * @author Miles Sabin
 */
abstract class CaseAux[-P, L <: HList] { outer =>
  type Result
  val value : L => Result
  
  def apply(t : L) = value(t)
  def apply()(implicit ev: HNil =:= L) = value(HNil)
  def apply[T](t: T)(implicit ev: (T :: HNil) =:= L) = value(t :: HNil)
  def apply[T, U](t: T, u: U)(implicit ev: (T :: U :: HNil) =:= L) = value(t :: u :: HNil)
  def apply[P <: Product](p : P)(implicit hl: HListerAux[P, L]) = value(hl(p))
}

object CaseAux extends CaseInst {
  import Poly._
  
  def apply[P, L <: HList, R](v : L => R) = new CaseAux[P, L] {
    type Result = R
    val value = v
  }
}

/**
 * Base trait for polymorphic values.
 * 
 * @author Miles Sabin
 */
trait Poly extends PolyApply with PolyCases {
  /** The type of the case representing this polymorphic function at argument types `L`. */
  type Case[L <: HList] = CaseAux[this.type, L]
  def Case[L <: HList, R](v : L => R) = new Case[L] {
    type Result = R
    val value = v
  }
  
  type Case0[T] = Pullback[HNil, T]
  
  def apply[R](implicit c : Case0[R]) : R = c()

  /** The type of a case of this polymorphic function of the form `L => R` */
  type Pullback[L <: HList, R] = Case[L] { type Result = R }
  
  /** The type of a case of this polymorphic function of the form `T => T` */
  type Hom[T] = Pullback1[T, T]

  def compose[F <: Poly](f: F) = new Compose[this.type, F](this, f)
  
  def andThen[F <: Poly](f: F) = new Compose[F, this.type](f, this)

  trait CaseBuilder[T, L <: HList, R] {
    def apply(t: T): Pullback[L, R]
  }
  
  trait LowPriorityCaseBuilder {
    implicit def valueCaseBuilder[T] = new CaseBuilder[T, HNil, T] {
      def apply(t: T) = Case((_: HNil) => t)
    }
  }
  
  object CaseBuilder extends LowPriorityCaseBuilder {
    implicit def fnCaseBuilder[F](implicit hl: FnHLister[F]) = new CaseBuilder[F, hl.Args, hl.Result] {
      def apply(f: F) = Case((l : hl.Args) => hl(f)(l))
    }
  }
  
  def use[T, L <: HList, R](t : T)(implicit cb: CaseBuilder[T, L, R]) = cb(t)
}

/**
 * Trait simplifying the creation of polymorphic values.
 */
trait Poly0 extends Poly {
  def at[T](t: T) = new Case[HNil] {
    type Result = T
    val value = (l : HNil) => t
  }
}

/**
 * Represents the composition of two polymorphic function values.
 *  
 * @author Miles Sabin
 */
class Compose[F <: Poly, G <: Poly](f : F, g : G) extends Poly

object Compose {
  import Poly._
  implicit def composeCase[F <: Poly, G <: Poly, T, U, V]
    (implicit cG : Pullback1Aux[G, T, U], cF : Pullback1Aux[F, U, V]) = new CaseAux[Compose[F, G], T :: HNil] {
    type Result = V
    val value = (t : T :: HNil) => cF(cG.value(t))
  }
}

/**
 * Provides implicit conversions from polymorphic function values to monomorphic function values, eg. for use as
 * arguments to ordinary higher order functions.
 *  
 * @author Miles Sabin
 */
object Poly extends PolyInst with PolyAuxCases {
  type PullbackAux[-P, L <: HList, R] = CaseAux[P, L] { type Result = R }
  type HomAux[-P, T] = PullbackAux[P, T :: HNil, T]
  
  implicit def inst0[P <: Poly, R](p : P)(implicit c : p.Case0[R]) : R = c()
  
  type Case0Aux[-P] = CaseAux[P, HNil]
  type Pullback0Aux[-P, T] = PullbackAux[P, HNil, T]
  def Case0Aux[P, T](v : T) = new CaseAux[P, HNil] {
    type Result = T
    val value = (l : HNil) => v
  }
}

/**
 * Base class for lifting a `Function1` to a `Poly1`
 */
class ->[T, R](f : T => R) extends Poly1 {
  implicit def subT[U <: T] = at[U](f)
}

trait LowPriorityLiftFunction1 extends Poly1 {
  implicit def default[T] = at[T](_ => HNil : HNil)
}

/**
 * Base class for lifting a `Function1` to a `Poly1` over the universal domain, yielding an `HList` with the result as
 * its only element if the argument is in the original functions domain, `HNil` otherwise. 
 */
class >->[T, R](f : T => R) extends LowPriorityLiftFunction1 {
  implicit def subT[U <: T] = at[U](f(_) :: HNil)
}

trait LowPriorityLiftU extends Poly {
  implicit def default[L <: HList] = new Case[L] {
    type Result = HNil
    val value = (l : L) => HNil
  }
}

/**
 * Base class for lifting a `Poly` to a `Poly` over the universal domain, yielding an `HList` with the result as it's
 * only element if the argument is in the original functions domain, `HNil` otherwise. 
 */
class LiftU[P <: Poly](p : P)  extends LowPriorityLiftU {
  implicit def defined[L <: HList](implicit caseT : CaseAux[P, L]) = new Case[L] {
    type Result = caseT.Result :: HNil
    val value = (l : L) => caseT(l) :: HNil
  } 
}

/**
 * Base trait for natural transformations.
 * 
 * @author Miles Sabin
 */
trait ~>[F[_], G[_]] extends Poly1 {
  def apply[T](f : F[T]) : G[T]
  implicit def caseUniv[T] = at[F[T]](apply(_))
}

object ~> {
  implicit def inst1[F[_], G[_], T](f : F ~> G) : F[T] => G[T] = f(_)
  implicit def inst2[G[_], T](f : Id ~> G) : T => G[T] = f(_)
  implicit def inst3[F[_], T](f : F ~> Id) : F[T] => T = f(_)
  implicit def inst4[T](f : Id ~> Id) : T => T = f[T](_)  // Explicit type argument needed here to prevent recursion?
  implicit def inst5[F[_], G, T](f : F ~> Const[G]#λ) : F[T] => G = f(_)
  implicit def inst6[G, T](f : Id ~> Const[G]#λ) : T => G = f(_)
}

trait ~>>[F[_], R] extends Poly1 {
  def apply[T](f : F[T]) : R
  implicit def caseUniv[T] = at[F[T]](apply(_))
}

object ~>> {
  implicit def inst1[F[_], R, T](f : F ~>> R) : F[T] => R = f(_)
  implicit def inst2[R, T](f : Id ~>> R) : T => R = f(_)
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
  def apply[T](t : T) = t
}

/** Polymorphic singleton function. */
object singleton extends (Id ~> Set) {
  def apply[T](t : T) = Set(t)
}

/** Polymorphic function selecting an arbitrary element from a non-empty `Set`. */
object choose extends (Set ~> Option) {
  def apply[T](s : Set[T]) = s.headOption 
}

/** Polymorphic function creating singleton `List`s. */
object list extends (Id ~> List) {
  def apply[T](t : T) = List(t)
}

/** Polymorphic function returning the head of a `List`. */
object headOption extends (List ~> Option) {
  def apply[T](l : List[T]) = l.headOption
}

/** Polymorphic function testing whether or not an `Option` is defined. */
object isDefined extends (Option ~>> Boolean) {
  def apply[T](o : Option[T]) = o.isDefined
}

/** Polymorphic function which opens an `Option`. */
object get extends (Option ~> Id) {
  def apply[T](o : Option[T]) = o.get
}

/** Polymorphic function which injects a value into an `Option`. */
object option extends (Id ~> Option) {
  def apply[T](t : T) = Option(t)
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
  implicit val zeroInt = at(0)
  implicit val zeroDouble = at(0.0)
  implicit val zeroString = at("")
  implicit def zeroList[T] = at[List[T]](Nil)
}
