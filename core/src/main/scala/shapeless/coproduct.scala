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

sealed trait :+:[+H, +T <: Coproduct] extends Coproduct

final case class Inl[+H, +T <: Coproduct](head : H) extends :+:[H, T] {
  override def toString = head.toString
}

final case class Inr[+H, +T <: Coproduct](tail : T) extends :+:[H, T] {
  override def toString = tail.toString
}

sealed trait CNil extends Coproduct

object Coproduct {
  class MkCoproduct[C <: Coproduct] {
    def apply[T](t: T)(implicit inj: Inject[C, T]): C = inj(t) 
  }
  
  def apply[C <: Coproduct] = new MkCoproduct[C]
  
  class CoproductOps[C <: Coproduct](c: C) {
    def map(f: Poly)(implicit mapper: CPMapper[f.type, C]): mapper.Out = mapper(c)
    
    def select[T](implicit selector: CPSelector[C, T]): Option[T] = selector(c)
    
    def unify[T](implicit unifier: CPUnifierAux[C, T]): T = (unifier(c): @unchecked) match {
      case Inl(h) => h
    }
  }
  implicit def cpOps[C <: Coproduct](c: C) = new CoproductOps(c) 
}

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

trait CPSelector[C <: Coproduct, T] {
  def apply(c: C): Option[T]
}

object CPSelector {
  implicit def tlSelector1[H, T <: Coproduct, S](implicit st: CPSelector[T, S]): CPSelector[H :+: T, S] = new CPSelector[H :+: T, S] {
    def apply(c: H :+: T): Option[S] = c match {
      case Inl(h) => None
      case Inr(t) => st(t)
    }
  }

  implicit def hdSelector[H, T <: Coproduct](implicit st: CPSelector[T, H] = null): CPSelector[H :+: T, H] = new CPSelector[H :+: T, H] {
    def apply(c: H :+: T): Option[H] = c match {
      case Inl(h) => Some(h)
      case Inr(t) => if (st != null) st(t) else None
    }
  }
}

trait CPMapper[F <: Poly, C <: Coproduct] {
  type Out <: Coproduct
  def apply(c: C): Out
}

object CPMapper {
  implicit def cpMapper[F <: Poly, C <: Coproduct, Out0 <: Coproduct](implicit mapper: CPMapperAux[F, C, Out0]) = new CPMapper[F, C] {
    type Out = Out0
    def apply(c: C): Out = mapper(c)
  }
}

trait CPMapperAux[F <: Poly, C <: Coproduct, Out] {
  def apply(c: C): Out
}

object CPMapperAux {
  import Poly._

  implicit def cnilMapper[F <: Poly]: CPMapperAux[F, CNil, CNil] = new CPMapperAux[F, CNil, CNil] {
    def apply(t: CNil): CNil = t
  }

  implicit def cpMapper[F <: Poly, H, OutH, T <: Coproduct, OutT <: Coproduct]
    (implicit fh: Pullback1Aux[F, H, OutH], mt: CPMapperAux[F, T, OutT]): CPMapperAux[F, H :+: T, OutH :+: OutT] =
      new CPMapperAux[F, H :+: T, OutH :+: OutT] {
        def apply(c: H :+: T): OutH :+: OutT = c match {
          case Inl(h) => Inl(fh(h))
          case Inr(t) => Inr(mt(t))
        }
      }
}

trait CPUnifier[C <: Coproduct] {
  type Out
  def apply(c: C): Out :+: CNil
}

object CPUnifier {
  implicit def cpUnifier[C <: Coproduct, Out0](implicit unifier: CPUnifierAux[C, Out0]): CPUnifier[C] = new CPUnifier[C] {
    type Out = Out0
    def apply(c: C): Out :+: CNil = unifier(c)
  }
}

trait CPUnifierAux[C <: Coproduct, Out] {
  def apply(c: C): Out :+: CNil
}

object CPUnifierAux {
  implicit def lstUnifier[H]: CPUnifierAux[H :+: CNil, H] = new CPUnifierAux[H :+: CNil, H] {
    def apply(c: H :+: CNil): H :+: CNil = c
  }
  
  implicit def cpUnifier[H, T <: Coproduct, TL, L](implicit ut: CPUnifierAux[T, TL], l: Lub[H, TL, L]): CPUnifierAux[H :+: T, L] = new CPUnifierAux[H :+: T, L] {
    def apply(c: H :+: T): L :+: CNil = c match {
      case Inl(h) => Inl(l.left(h))
      case Inr(t) => ut(t) match {
        case Inl(h) => Inl(l.right(h))
        case Inr(t) => Inr(t)
      }
    }
  }
}
