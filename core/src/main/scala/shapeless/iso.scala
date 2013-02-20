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

package shapeless

/**
 * Representation of an isomorphism between a type (typically a case class) and an `HList`.
 */
trait Iso[T, U] { self =>
  def to(t : T) : U
  def from(u : U) : T

  def reverse : Iso[U, T] = new Iso[U, T] {
    def to(u : U) : T = self.from(u)
    def from(t : T) : U = self.to(t)

    override def reverse = self
  }
}

trait LowPriorityIso {
  import Functions._
  
  implicit def identityIso[T] = new Iso[T, T] {
    def to(t : T) : T = t
    def from(t : T) : T = t
  }
  def hlist[CC, C, T <: Product, L <: HList](apply : C, unapply : CC => Option[T])
    (implicit fhl : FnHListerAux[C, L => CC], hl : HListerAux[T, L]) =
      new Iso[CC, L] {
        val ctor = apply.hlisted
        val dtor = (cc : CC) => hl(unapply(cc).get)
        def to(t : CC) : L = dtor(t)
        def from(l : L) : CC = ctor(l)
      }
}

object Iso extends LowPriorityIso {
  import Functions._
  import Tuples._

  // Special case for one-element cases classes because their unapply result types
  // are Option[T] rather than Option[Tuple1[T]] which would be required to fit
  // the general case.
  def hlist[CC, T](apply : T => CC, unapply : CC => Option[T]) =
    new Iso[CC, T :: HNil] {
      val ctor = apply.hlisted
      val dtor = (cc : CC) => unapply(cc).get :: HNil 
        def to(t : CC) : T :: HNil = dtor(t)
        def from(l : T :: HNil) : CC = ctor(l)
      }

  implicit def tupleHListIso[T <: Product, L <: HList](implicit hl : HListerAux[T, L], uhl : TuplerAux[L, T]) =
    new Iso[T, L] {
      val ctor = uhl.apply _
      val dtor = hl.apply _
      def to(t : T) : L = dtor(t)
      def from(l : L) : T = ctor(l)
    }
  
  implicit def fnHListFnIso[F, L <: HList, R](implicit hl : FnHListerAux[F, L => R], unhl : FnUnHListerAux[L => R, F]) =
    new Iso[F, L => R] {
      def to(f : F) : L => R = hl(f)
      def from(l : L => R) = unhl(l)
    }
}

// vim: expandtab:ts=2:sw=2
