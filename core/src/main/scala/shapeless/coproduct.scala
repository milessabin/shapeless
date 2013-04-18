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

sealed trait Coproduct

trait :+:[+A, +B] extends Coproduct

case class Inl[A](a: A) extends (A :+: Nothing)
case class Inr[B](b: B) extends (Nothing :+: B)

object Coproduct {
  class MkCoproduct[C] {
    def apply[T](t: T)(implicit inj: Inject[C, T]): C = inj(t) 
  }
  
  def apply[C] = new MkCoproduct[C]
  
  class CoproductOps[C](c: C) {
    def map[F <: Poly](f: F)(implicit mapper: CPMapper[F, C]): mapper.Out = mapper(c)
    
    def select[T](implicit selector: CPSelector[C, T]): Option[T] = selector(c)
    
    def unify[T](implicit unifier: CPUnifierAux[C, T]): T = unifier(c)
  }
  implicit def cpOps[C](c: C) = new CoproductOps(c) 
}

trait Inject[C, I] {
  def apply(i: I): C
}

trait LowPriorityInject {
  implicit def lstInject0[T]: Inject[T, T] = new Inject[T, T] {
    def apply(t: T): T = t
  }
  
  implicit def lstInject1[H, I]: Inject[H :+: I, I] = new Inject[H :+: I, I] {
    def apply(t: I): H :+: I = Inr(t) 
  }
  
  implicit def tlInject[H, T, I](implicit tlInj : Inject[T, I]): Inject[H :+: T, I] = new Inject[H :+: T, I] {
    def apply(i: I): H :+: T = Inr(tlInj(i))
  }
}

object Inject extends LowPriorityInject {
  implicit def hdInject[I, CT]: Inject[I :+: CT, I] = new Inject[I :+: CT, I] {
    def apply(i: I): I :+: CT = Inl(i) 
  }
}

trait CPSelector[C, T] {
  def apply(c: C): Option[T]
}

trait LowPriorityCPSelector {
  implicit def tlSelector0[S]: CPSelector[S, S] = new CPSelector[S, S] {
    def apply(c: S): Option[S] = Some(c)
  }
  
  implicit def tlSelector1[H, T, S](implicit st: CPSelector[T, S]): CPSelector[H :+: T, S] = new CPSelector[H :+: T, S] {
    def apply(c: H :+: T): Option[S] = c match {
      case Inl(h) => None
      case Inr(t) => st(t)
    }
  }
}

object CPSelector extends LowPriorityCPSelector {
  implicit def hdSelector[H, T](implicit st: CPSelector[T, H] = null): CPSelector[H :+: T, H] = new CPSelector[H :+: T, H] {
    def apply(c: H :+: T): Option[H] = c match {
      case Inl(h) => Some(h)
      case Inr(t) => if (st != null) st(t) else None
    }
  }
}

trait CPMapper[F <: Poly, C] {
  type Out
  def apply(c: C): Out
}

object CPMapper {
  implicit def cpMapper[F <: Poly, C, Out0](implicit mapper: CPMapperAux[F, C, Out0]) = new CPMapper[F, C] {
    type Out = Out0
    def apply(c: C): Out = mapper(c)
  }
}

trait CPMapperAux[F <: Poly, C, Out] {
  def apply(c: C): Out
}

trait LowPriorityCPMapperAux {
  import Poly._

  implicit def tlMapper[F <: Poly, T, OutT](implicit ft: Pullback1Aux[F, T, OutT]): CPMapperAux[F, T, OutT] = new CPMapperAux[F, T, OutT] {
    def apply(t: T): OutT = ft(t)
  }
}

object CPMapperAux extends LowPriorityCPMapperAux {
  import Poly._
  
  implicit def hdMapper[F <: Poly, H, OutH, T, OutT]
    (implicit fh: Pullback1Aux[F, H, OutH], mt: CPMapperAux[F, T, OutT]): CPMapperAux[F, H :+: T, OutH :+: OutT] =
      new CPMapperAux[F, H :+: T, OutH :+: OutT] {
        def apply(c: H :+: T): OutH :+: OutT = c match {
          case Inl(h) => Inl(fh(h))
          case Inr(t) => Inr(mt(t))
        }
      }
}

trait CPUnifier[C] {
  type Out
  def apply(c: C): Out
}

object CPUnifier {
  implicit def cpUnifier[C, Out0](implicit unifier: CPUnifierAux[C, Out0]): CPUnifier[C] = new CPUnifier[C] {
    type Out = Out0
    def apply(c: C): Out = unifier(c)
  }
}

trait CPUnifierAux[C, Out] {
  def apply(c: C): Out
}

trait LowPriorityCPUnifierAux {
  implicit def tlUnifier[T]: CPUnifierAux[T, T] = new CPUnifierAux[T, T] {
    def apply(t: T): T = t
  }
}

object CPUnifierAux extends LowPriorityCPUnifierAux {
  implicit def hdUnifier[H, T, TL, L](implicit ut: CPUnifierAux[T, TL], l: Lub[H, TL, L]): CPUnifierAux[H :+: T, L] = new CPUnifierAux[H :+: T, L] {
    def apply(c: H :+: T): L = c match {
      case Inl(h) => l.left(h)
      case Inr(t) => l.right(ut(t))
    }
  }
}
