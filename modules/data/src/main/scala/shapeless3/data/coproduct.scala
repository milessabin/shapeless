/*
 * Copyright (c) 2013-19 Miles Sabin
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

package shapeless3.data

/** Encodes a coproduct type, such as a sealed family of case classes.
 */
sealed trait Coproduct extends Product with Serializable

/** Like Either, the :+: type defines a new type that can contain either H or T.
 */
sealed trait :+:[+H, +T <: Coproduct] extends Coproduct {
  /**
   * Non-recursive fold (like Either#fold)
   */
  def eliminate[A](l: H => A, r: T => A): A
}

/** `H :+: T` can either be `H` or `T`.
  * In this case it is `H`.
  */
final case class Inl[+H, +T <: Coproduct](head : H) extends :+:[H, T] {
  override def eliminate[A](l: H => A, r: T => A) = l(head)
}

/** `H :+: T` can either be `H` or `T`.
  * In this case it is `T`.
  */
final case class Inr[+H, +T <: Coproduct](tail : T) extends :+:[H, T] {
  override def eliminate[A](l: H => A, r: T => A) = r(tail)
}

/** The CNil type is used to terminate a 'list' of :+: alternatives.
  *
  * Like the Nil constructor of List, it does not convey real information.
  * This is achieved by not having any value for CNil.
  *
  * This makes the type `Int :+: CNil` equivalent to `Int`, because the right (`Inr`) alternative
  * of `:+:` can not be constructed properly.
  */
sealed trait CNil extends Coproduct {
  /** Call this when you hit the CNil case in pattern matching to make the match exhaustive and safe. */
  def impossible: Nothing
}

object Coproduct {
  import ops.coproduct.Inject

  class MkCoproduct[C <: Coproduct] {
    def apply[T](t: T)(implicit inj: Inject[C, T]): C = inj(t)
  }

  def apply[C <: Coproduct] = new MkCoproduct[C]
}

object ops {
  object coproduct {

    /**
      * Type class for converting a value of type I into the coproduct C. (Type I need to occur in the coproduct C)
      */
    trait Inject[C <: Coproduct, I] extends Serializable {
      def apply(i: I): C
    }

    object Inject {
      def apply[C <: Coproduct, I](implicit inject: Inject[C, I]): Inject[C, I] = inject

      implicit def tlInject[H, T <: Coproduct, I](implicit tlInj : Inject[T, I]): Inject[H :+: T, I] = new Inject[H :+: T, I] {
        def apply(i: I): H :+: T = Inr(tlInj(i))
      }

      implicit def hdInject[H, HH <: H, T <: Coproduct]: Inject[H :+: T, HH] = new Inject[H :+: T, HH] {
        def apply(i: HH): H :+: T = Inl(i)
      }
    }
  }
}
