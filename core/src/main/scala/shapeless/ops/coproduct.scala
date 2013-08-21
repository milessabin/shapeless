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

package shapeless
package ops

import poly._

object coproduct {
  trait Inject[C <: Coproduct, I] {
    def apply(i: I): C
  }

  object Inject {
    implicit def tlInject[H, T <: Coproduct, I](implicit tlInj : Inject[T, I]): Inject[H :+: T, I] = new Inject[H :+: T, I] {
      def apply(i: I): H :+: T = Inr(tlInj(i))
    }

    implicit def hdInject[H, T <: Coproduct]: Inject[H :+: T, H] = new Inject[H :+: T, H] {
      def apply(i: H): H :+: T = Inl(i)
    }
  }

  trait Selector[C <: Coproduct, T] {
    def apply(c: C): Option[T]
  }

  object Selector {
    implicit def tlSelector1[H, T <: Coproduct, S](implicit st: Selector[T, S]): Selector[H :+: T, S] = new Selector[H :+: T, S] {
      def apply(c: H :+: T): Option[S] = c match {
        case Inl(h) => None
        case Inr(t) => st(t)
      }
    }

    implicit def hdSelector[H, T <: Coproduct](implicit st: Selector[T, H] = null): Selector[H :+: T, H] = new Selector[H :+: T, H] {
      def apply(c: H :+: T): Option[H] = c match {
        case Inl(h) => Some(h)
        case Inr(t) => if (st != null) st(t) else None
      }
    }
  }

  trait Mapper[F <: Poly, C <: Coproduct] extends DepFn1[C] { type Out <: Coproduct }

  object Mapper {
    type Aux[F <: Poly, C <: Coproduct, Out0 <: Coproduct] = Mapper[F, C] { type Out = Out0 }

    implicit def cnilMapper[F <: Poly]: Aux[F, CNil, CNil] = new Mapper[F, CNil] {
      type Out = CNil
      def apply(t: CNil): Out = t
    }

    implicit def cpMapper[F <: Poly, H, OutH, T <: Coproduct]
      (implicit fh: Case1.Aux[F, H, OutH], mt: Mapper[F, T]): Aux[F, H :+: T, OutH :+: mt.Out] =
        new Mapper[F, H :+: T] {
          type Out = OutH :+: mt.Out
          def apply(c: H :+: T): Out = c match {
            case Inl(h) => Inl(fh(h))
            case Inr(t) => Inr(mt(t))
          }
        }
  }

  trait Unifier[C <: Coproduct] extends DepFn1[C]

  object Unifier {
    type Aux[C <: Coproduct, Out0] = Unifier[C] { type Out = Out0 }

    implicit def lstUnifier[H]: Aux[H :+: CNil, H] =
      new Unifier[H :+: CNil] {
        type Out = H
        def apply(c: H :+: CNil): Out = (c: @unchecked) match {
          case Inl(h) => h
        }
      }
    
    implicit def cpUnifier[H1, H2, T <: Coproduct, TL, L, Out0 >: L]
      (implicit u: Lub[H1, H2, L], lt: Aux[L :+: T, Out0]): Aux[H1 :+: H2 :+: T, Out0] =
        new Unifier[H1 :+: H2 :+: T] {
          type Out = Out0
          def apply(c: H1 :+: H2 :+: T): Out = c match {
            case Inl(h1) => u.left(h1)
            case Inr(Inl(h2)) => u.right(h2)
            case Inr(Inr(t)) => lt(Inr(t))
          }
        }
  }
}
