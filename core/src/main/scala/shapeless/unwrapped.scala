/*
 * Copyright (c) 2016 Miles Sabin
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

import newtype._

trait Unwrapped[W] extends Serializable {
  type U
  def unwrap(w: W): U
  def wrap(u: U): W
}

object Unwrapped extends UnwrappedInstances {
  type Aux[W, U0] = Unwrapped[W] { type U = U0 }
}

trait UnwrappedInstances extends LowPriorityUnwrappedInstances {
  implicit def unwrapAnyVal[W <: AnyVal, Repr, UI, UF](implicit
    gen: Generic.Aux[W, Repr],
    avh: AnyValHelper.Aux[Repr, UI],
    chain: Strict[Unwrapped.Aux[UI, UF]]
  ) = new Unwrapped[W] {
    type U = UF
    def unwrap(w: W): U = chain.value.unwrap(avh.unwrap(gen.to(w)))
    def wrap(u: U): W = gen.from(avh.wrap(chain.value.wrap(u)))
  }

  sealed trait AnyValHelper[Repr] extends Serializable {
    type U
    def unwrap(r: Repr): U
    def wrap(u: U): Repr
  }
  object AnyValHelper {
    type Aux[Repr, U0] = AnyValHelper[Repr] { type U = U0 }
    implicit def sizeOneHListHelper[T] = new AnyValHelper[T :: HNil] {
      type U = T
      def unwrap(hl: T :: HNil): T = hl.head
      def wrap(t: T): T :: HNil = t :: HNil
    }
  }

  implicit def newtypeUnwrapped[UI, Ops, UF](implicit chain: Strict[Unwrapped.Aux[UI, UF]])=
    new Unwrapped[Newtype[UI, Ops]] {
      type U = UF
      def unwrap(n: Newtype[UI, Ops]): UF = chain.value.unwrap(n.asInstanceOf[UI])
      def wrap(u: UF): Newtype[UI, Ops] = chain.value.wrap(u).asInstanceOf[Newtype[UI, Ops]]
    }

}

trait LowPriorityUnwrappedInstances {
  implicit def selfUnwrapped[T] =
    new Unwrapped[T] {
      type U = T
      def unwrap(t: T) = t
      def wrap(t: T) = t
    }
}
