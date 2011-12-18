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

/**
 * Type-specific case of a polymorphic value.
 * 
 * @author Miles Sabin
 */
case class Case[-P, T](t : T) {
  /** The monomorphic value at `T`. */
  def value = t
  /** If there is evidence that `T` is a function type, then apply it. */
  def apply[F, G](f : F)(implicit ev : T <:< (F => G)) : G = t(f)
}

/**
 * Base trait for polymorphic values.
 * 
 * @author Miles Sabin
 */
trait Poly {
  /** The common outer type constructor shared by all type-specific case of this polymorphic value. */
  type TC[_]

  /** The type of the `Case` representing this polymorphic value at type `TC[T]`. */
  type λ[T] = Case[this.type, TC[T]]

  /** Creates an instance of the `Case` representing this polymorphic value at type `TC[T]`. */
  def λ[T](c : TC[T]) = Case[this.type, TC[T]](c)
}

/**
 * Trait representing non-function polymorphic values.
 * 
 * @author Miles Sabin
 */
trait PolyVal[TC0[_]] extends Poly {
  type TC[X] = TC0[X]
  def apply[T](implicit c : λ[T]) : TC[T] = c.value
}

/**
 * Trait representing polymorphic function values.
 * 
 * @author Miles Sabin
 */
trait HRFn extends Poly {
  /** The common form of this polymorphic function value in all type specific cases. */
  type TC[T] = F[T] => G[T]
  
  /**
   * The common outer type constructor shared by the argument type of this polymorphic
   * function value in all type specific cases.
   * */
  type F[_]
  /**
   * The common outer type constructor shared by the result type of this polymorphic
   * function value in all type specific cases.
   * */
  type G[_]

  /** Implementation of the default case for this polymorphic function value */
  def default[T](f : F[T]) : G[T]
  
  /** The default case for this polymorphic function value */
  implicit def defaultCase[T] = λ[T](default)
  
  /** Apply this polymorphic function value using the appropriate type-specific case. */
  def apply[T](f : F[T])(implicit c : λ[T] = defaultCase[T]) : G[T] = c(f)
}

/**
 * Convenience trait for extension by polymorphic function value implementations.
 * 
 * @author Miles Sabin
 */
trait ~>[F0[_], G0[_]] extends HRFn {
  type F[X] = F0[X]
  type G[X] = G0[X]
}

/**
 * Mixin for polymorphic values which should have all their type specific cases covered by implicitly provided `Case`s
 * rather than by the default case.
 * 
 * @author Miles Sabin
 */
trait NoDefault extends HRFn {
  def default[T](f : F[T]) : G[T] = {
    sys.error("No default case for: "+getClass.getName+"@"+f.getClass.getName)
  }
}

/**
 * Provides an implicit conversion from polymorphic function values to monomorphic function values, eg. for use as
 * arguments to ordinary higher order functions.
 * 
 * Includes miscellaneous polymorphic function definitions.
 *  
 * @author Miles Sabin
 */
object Poly {
  import TypeOperators._
  
  /**
   * Instantiates a polymorphic function value as an ordinary monomophic function value at a specific type.
   */
  implicit def univInstFn[HF <: HRFn, T](h : HF)(implicit c : h.λ[T] = h.defaultCase[T]) : h.TC[T] = c.value

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
  object isDefined extends (Option ~> Const[Boolean]#λ) {
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
  
  /** Polymorphic zero with type specific cases. */
  object zero extends PolyVal[Id]
  implicit val intZero = zero.λ[Int](0) 
  implicit val stringZero = zero.λ[String]("") 
  implicit def listZero[T] = zero.λ[List[T]](Nil) 
}
