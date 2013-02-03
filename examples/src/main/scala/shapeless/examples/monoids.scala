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

object MonoidExamples extends App {
  import shapeless._
  import HList._
  import Monoid._

  // Given an isomorphism between `C` and an `HList` `L`, construct a monoid instance for `C` given
  // the monoid instance for `L`, which is in turn derived from the monoid instances for its/`C`'s
  // element types.
  implicit def ccMonoid[C, L <: HList](implicit iso : Iso[C, L], ml : Monoid[L]) = new Monoid[C] {
    def zero = iso.from(ml.zero)
    def append(a : C, b : C) = iso.from(iso.to(a) |+| iso.to(b))
  }

  // A pair of arbitrary case classes
  case class Foo(i : Int, s : String)
  case class Bar(b : Boolean, s : String, d : Double)

  // Automatically they're monoids ...
  
  implicitly[Monoid[Foo]]
  val f = Foo(13, "foo") |+| Foo(23, "bar")
  assert(f == Foo(36, "foobar"))

  implicitly[Monoid[Bar]]
  val b = Bar(true, "foo", 1.0) |+| Bar(false, "bar", 3.0)
  assert(b == Bar(true, "foobar", 4.0))
}

/**
 * Pedagogic subset of the Scalaz monoid
 */
trait Monoid[T] {
  def zero : T
  def append(a : T, b : T) : T
}

object Monoid {
  import shapeless._
  import HList._
  
  def mzero[T](implicit mt : Monoid[T]) = mt.zero
  
  trait MonoidOps[T] {
    def |+|(b : T) : T
  }
  
  implicit def monoidOps[T](a : T)(implicit mt : Monoid[T]) : MonoidOps[T] = new MonoidOps[T] {
    def |+|(b : T) = mt.append(a, b)
  }
  
  implicit def booleanMonoid : Monoid[Boolean] = new Monoid[Boolean] {
    def zero = false
    def append(a : Boolean, b : Boolean) = a || b
  }
  
  implicit def intMonoid : Monoid[Int] = new Monoid[Int] {
    def zero = 0
    def append(a : Int, b : Int) = a+b
  }
  
  implicit def doubleMonoid : Monoid[Double] = new Monoid[Double] {
    def zero = 0.0
    def append(a : Double, b : Double) = a+b
  }
  
  implicit def stringMonoid : Monoid[String] = new Monoid[String] {
    def zero = ""
    def append(a : String, b : String) = a+b
  }

  implicit def hnilMonoid : Monoid[HNil] = new Monoid[HNil] {
    def zero = HNil
    def append(a : HNil, b : HNil) = HNil
  }
  
  implicit def hlistMonoid[H, T <: HList](implicit mh : Monoid[H], mt : Monoid[T]) : Monoid[H :: T] = new Monoid[H :: T] {
    def zero = mh.zero :: mt.zero
    def append(a : H :: T, b : H :: T) = (a.head |+| b.head) :: (a.tail |+| b.tail)  
  }
}
