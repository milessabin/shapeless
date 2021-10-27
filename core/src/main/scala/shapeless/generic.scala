/*
 * Copyright (c) 2012-18 Lars Hupel, Miles Sabin
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

import scala.annotation.StaticAnnotation

 /** Represents the ability to convert from a concrete type (e.g. a case class)
  * to a generic ([[HList]] / [[Coproduct]]} based) representation of the type.
  *
  * For example:
  * {{{
  * scala> sealed trait Animal

  * defined trait Animal
  * scala> case class Cat(name: String, livesLeft: Int) extends Animal
  * defined class Cat
  *
  * scala> case class Dog(name: String, bonesHidden: Int) extends Animal
  * defined class Dog
  *
  * scala> val genCat = Generic[Cat]
  * genCat: shapeless.Generic[Cat]{ type Repr = String :: Int :: HNil } = ...
  *
  * scala> val genDog = Generic[Dog]
  * genDog: shapeless.Generic[Dog]{ type Repr = String :: Int :: HNil } = ...
  *
  * scala> val garfield = Cat("Garfield", 9)
  * garfield: Cat = Cat(Garfield,9)
  *
  * scala> val genGarfield = genCat.to(garfield)
  * genGarfield: genCat.Repr = Garfield :: 9 :: HNil
  *
  * scala> val reconstructed = genCat.from(genGarfield)
  * reconstructed: Cat = Cat(Garfield,9)
  *
  * scala> reconstructed == garfield
  * res0: Boolean = true
  *
  * }}}
  *
  * Note that constituents of Cat and Dog are exactly the same - a String and an Int. So we could do:
  *
  * {{{
  *
  * scala> val odieAsCat = genCat.from(genDog.to(odie))
  * odieAsCat: Cat = Cat(odie,3)
  *
  * }}}
  *
  * This is quite useful in certain cases, such as copying from one object type to another, as in schema evolution.
  *
  * Note that the generic representation depends on the type at which we instantiate Generic. In the
  * example above we instantiated it at Cat and at Dog, and so the generic representation gave the minimal constituents
  * of each of those.
  *
  * However, if we instantiate Generic[Animal] instead the generic representation would encode
  * the Cat-ness or Dog-ness of the instance as well (see [[Coproduct]] for details of the encoding):
  *
  * {{{
  *
  * scala> genDog.to(odie)
  * res9: genDog.Repr = odie :: 3 :: HNil
  *
  * scala> val genAnimal = Generic[Animal]
  * genAnimal: shapeless.Generic[Animal]{ type Repr = Cat :+: Dog :+: CNil } = ...
  *
  * scala> genAnimal.to(odie)
  * res8: genAnimal.Repr = Dog(odie,3)
  *
  * scala> genAnimal.to(odie) match { case Inr(Inl(dog)) => dog; case _ => null }
  * res9: Dog = Dog(odie,3)
  *
  * }}}
  *
  * Inr and Inl are [[shapeless.Coproduct]] constructors.
  * Shapeless constructs each class representation as a sort of
  * "nested Either" using Coproduct. So in our example, genAnimal would essentially encode garfield as Inl(garfield)
  * and odie as Inr(Inl(odie)). Please see [[shapeless.Coproduct]] for more details.
  * }}}
  *
  * @tparam T  An immutable data type that has a canonical way of constructing and deconstructing
  *            instances (e.g. via apply / unapply). Sealed families of case classes work best.
  */
trait Generic[T] extends Serializable {
  /** The generic representation type for {T}, which will be composed of {Coproduct} and {HList} types  */
  type Repr

  /** Convert an instance of the concrete type to the generic value representation */
  def to(t : T) : Repr

  /** Convert an instance of the generic representation to an instance of the concrete type */
  def from(r : Repr) : T
}

/** The companion object for the [[Generic]] trait provides a way of obtaining a Generic[T] instance
 * for some T. In addition, it defines [[Generic.Aux]], which is an important implementation technique
 * that can be generally useful.
 */
object Generic extends GenericScalaCompat {

  /** Provides a representation of Generic[T], which has a nested Repr type, as a type with two type
   * parameters instead.
   *
   * This is useful for two reasons. First, it's surprisingly easy to wind up with a Generic type that
   * has lost the refinement that carries the crucial Generic.Repr type, a problem which Generic.Aux prevents.
   *
   * More importantly, Aux allows us to write code like this:
   *
   * {{{
   *   def myMethod[T, R]()(implicit eqGen: Generic.Aux[T,R], repEq: Eq[R]) = ???
   * }}}
   *
   * Here, we specify T, and we find a Generic.Aux[T,R] by implicit search. We then use R in the second argument.
   * Generic.Aux[T, R] is exactly equivalent to Generic[T] { type Repr = R }, but Scala doesn't allow us to write
   * it this way:
   *
   * {{{
   *   def myMethod[T, R]()(eqGen: Generic[T] { Repr = R }, reqEq: Eq[egGen.Repr]) = ???
   * }}}
   *
   * The reason is that we are not allowed to have dependencies between arguments in the same parameter group. So
   * Aux neatly sidesteps this problem.
   *
   * The "Aux pattern" is now in use in several other libraries as well, and is a useful general technique.
   *
   * @tparam T the type for which we want to find a Generic
   * @tparam Repr0 the generic representation type equivalent to T.
   */
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

  /** Provides an instance of Generic. Prefer this over finding one with `implicitly`, or else use `the`.
    *
    * Either of these approaches preserves the Repr type refinement, which `implicitly` will lose.
    */
  def apply[T](implicit gen: Generic[T]): Aux[T, gen.Repr] = gen

  /** Creates a new Generic instance from a pair of functions.
    *
    * The functions `f` and `g` should be the inverse of each other, i.e.
    *   - `f(g(x)) == x`
    *   - `g(f(y)) == y`
    */
  def instance[T, R](f: T => R, g: R => T): Aux[T, R] = new Generic[T] {
    type Repr = R
    def to(t: T): R = f(t)
    def from(r: R): T = g(r)
  }
}

/**
  * LabelledGeneric is similar to Generic, but includes information about field
  * names or class names in addition to the raw structure.
  *
  * Continuing the example from [[shapeless.Generic]], we use LabelledGeneric to convert an object to an [[shapeless.HList]]:
  *
  * {{{
  * scala> val lgenDog = LabelledGeneric[Dog]
  * lgenDog: shapeless.LabelledGeneric[Dog]{ type Repr = Record.`'name -> String, 'bonesHidden -> Int`.T } = ...
  *
  * scala> lgenDog.to(odie)
  * res15: lgenDog.Repr = odie :: 3 :: HNil
  * }}}
  *
  * Note that the representation does not include the labels! The labels are actually encoded in the generic type representation
  * using [[shapeless.Witness]] types.
  *
  * As with [[shapeless.Generic]], the representation for Animal captures the subclass embedding rather than the fields in the class,
  * using [[shapeless.Coproduct]]:
  *
  * {{{
  * scala> val lgenAnimal = LabelledGeneric[Animal]
  * lgenAnimal: shapeless.LabelledGeneric[Animal]{ type Repr = Union.`'Cat -> Cat, 'Dog -> Dog`.T } = ...
  *
  * scala> lgenAnimal.to(odie)
  * res16: lgenAnimal.Repr = Dog(odie,3)
  *
  * scala> genAnimal.to(odie) match { case Inr(Inl(dog)) => dog ; case _ => ???}
  * res19: Dog = Dog(odie,3)
  *
  * }}}
  *
  * @tparam T the type which this instance can convert to and from a labelled generic representation
  */
trait LabelledGeneric[T] extends Serializable {
  /** The generic representation type for {T}, which will be composed of {Coproduct} and {HList} types  */
  type Repr

  /** Convert an instance of the concrete type to the generic value representation */
  def to(t : T) : Repr

  /** Convert an instance of the generic representation to an instance of the concrete type */
  def from(r : Repr) : T
}

object LabelledGeneric extends LabelledGenericScalaCompat {

  /** Like [[shapeless.Generic.Aux]], this is an implementation of the Aux pattern, please
    * see comments there.
    * @tparam T the type
    * @tparam Repr0 the labelled generic representation of the type
    */
  type Aux[T, Repr0] = LabelledGeneric[T] { type Repr = Repr0 }

  /** Provides an instance of LabelledGeneric for the given T. As with [[shapeless.Generic]],
    * use this method or {{{the[LabelledGeneric[T]]}}} to obtain an instance for suitable given T. */
  def apply[T](implicit lgen: LabelledGeneric[T]): Aux[T, lgen.Repr] = lgen

  def unsafeInstance[T, R](gen: Generic[T]): Aux[T, R] = new LabelledGeneric[T] {
    type Repr = R
    def to(t: T): Repr = gen.to(t).asInstanceOf[R]
    def from(r: Repr): T = gen.from(r.asInstanceOf[gen.Repr])
  }
}

class nonGeneric extends StaticAnnotation

class IsTuple[T] extends Serializable

object IsTuple extends IsTupleScalaCompat

class HasProductGeneric[T] extends Serializable

object HasProductGeneric extends HasProductGenericScalaCompat

class HasCoproductGeneric[T] extends Serializable

object HasCoproductGeneric extends HasCoproductGenericScalaCompat
