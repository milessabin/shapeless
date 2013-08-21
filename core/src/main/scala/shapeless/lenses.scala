/*
 * Copyright (c) 2012-13 Miles Sabin 
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

import ops.hlist.{ At, Init, Last, Prepend, ReplaceAt, Tupler }

trait Lens[C, F] {
  outer =>
  def get(c : C) : F
  def set(c : C)(f : F) : C
  def modify(c : C)(f : F => F) : C = set(c)(f(get(c)))
  
  def compose[D](g : Lens[D, C]) = new Lens[D, F] {
    def get(d : D) : F = outer.get(g.get(d))
    def set(d : D)(f : F) : D = g.set(d)(outer.set(g.get(d))(f))
  }

  def >>[L <: HList](n : Nat)(implicit gen : Generic.Aux[F, L], lens : HListNthLens[L, n.N]) =
    new Lens[C, lens.Elem] {
      def get(c : C) : lens.Elem = lens.get(gen.to(outer.get(c)))
      def set(c : C)(f : lens.Elem) = outer.set(c)(gen.from(lens.set(gen.to(outer.get(c)))(f)))
    }
  
  def ~[G](other : Lens[C, G]) = new ProductLens[C, (F, G)] {
    def get(c : C) : (F, G) = (outer.get(c), other.get(c))
    def set(c : C)(fg : (F, G)) = other.set(outer.set(c)(fg._1))(fg._2)
  }
}

trait ProductLens[C, P <: Product] extends Lens[C, P] {
  outer =>
  def ~[T, L <: HList, LT <: HList, Q <: Product, QL <: HList](other : Lens[C, T])
    (implicit
      genp : Generic.Aux[P, L],
      tpp  : Tupler.Aux[L, P],
      pre  : Prepend.Aux[L, T :: HNil, LT],
      tpq  : Tupler.Aux[LT, Q],
      genq : Generic.Aux[Q, QL],
      init : Init.Aux[QL, L],
      last : Last.Aux[QL, T]) =
      new ProductLens[C, Q] {
        def get(c : C) : Q = (genp.to(outer.get(c)) :+ other.get(c)).tupled
        def set(c : C)(q : Q) = {
          val l = genq.to(q)
          other.set(outer.set(c)(l.init.tupled))(l.last)
        }
      }
}

object Lens {
  def apply[C] = id[C]
  
  object compose extends Poly2 {
    implicit def default[A, B, C] = at[Lens[B, C], Lens[A, B]](_ compose _)
  }

  def id[C] = new Lens[C, C] {
    def get(c : C) : C = c
    def set(c : C)(f : C) : C = f
  }
  
  def setLens[E](e : E) = new Lens[Set[E], Boolean] {
    def get(s : Set[E]) = s contains e
    def set(s : Set[E])(b : Boolean) = if(b) s+e else s-e
  }
  
  def mapLens[K, V](k : K) = new Lens[Map[K, V], Option[V]] {
    def get(m : Map[K, V]) = m get k
    def set(m : Map[K, V])(ov : Option[V]) = ov match {
      case Some(v) => m+(k -> v)
      case None => m-k
    } 
  }
  
  def hlistNthLens[L <: HList, N <: Nat](implicit lens : HListNthLens[L, N]) = lens.toLens
}

trait HListNthLens[L <: HList, N <: Nat] {
  type Elem
  def get(l : L) : Elem
  def set(l : L)(e : Elem) : L
  def toLens : Lens[L, Elem]
}

object HListNthLens {
  implicit def hlistNthLens[L <: HList, N <: Nat, E](implicit lens : HListNthLensAux[L, N, E]) =
    new HListNthLens[L, N] {
      type Elem = E
      def get(l : L) : Elem = lens.get(l)
      def set(l : L)(e : Elem) : L = lens.set(l)(e)
      def toLens : Lens[L, Elem] = lens
    }
}

trait HListNthLensAux[L <: HList, N <: Nat, E] extends Lens[L, E]

object HListNthLensAux {
  implicit def hlistNthLens[L <: HList, N <: Nat, E](implicit atx : At.Aux[L, N, E], replace : ReplaceAt.Aux[L, N, E, (E, L)]) =
    new HListNthLensAux[L, N, E] {
      def get(l : L) : E = l[N] 
      def set(l : L)(e : E) : L = l.updatedAt[N](e)
    }
}
