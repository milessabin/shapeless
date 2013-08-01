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

import ops.hlist.Length

/**
 * Type class witnessing the arity of a `Product`
 * 
 * @author Miles Sabin
 */
@deprecated("Use ProductLength instead", "2.0.0")
trait ProductArity[P <: Product] {
  type N <: Nat
}

@deprecated("Use ProductLength.Aux instead", "2.0.0")
trait ProductArityAux[P <: Product, N <: Nat]

/**
 * `ProductArity` type class instances.
 * 
 * @author Miles Sabin
 */
object ProductArity {
  implicit def arity[P <: Product, N0 <: Nat](implicit an : ProductArityAux[P, N0]) = new ProductArity[P] {
    type N = N0
  }
}

object ProductArityAux {
  implicit def arityN[P <: Product, L <: HList, N <: Nat]
    (implicit gen : Generic.Aux[P, L], len : Length.Aux[L, N]) = new ProductArityAux[P, N] {} 
}

/**
 * Type class supporting conversion of tuples to `HLists`.
 * 
 * @author Miles Sabin
 */
@deprecated("Use Generic instead", "2.0.0")
trait HLister[-T] {
  type Out <: HList
  def apply(t: T): Out
}
  
@deprecated("Use Generic instead", "2.0.0")
trait HListerAux[-T, Out <: HList] {
  def apply(t: T): Out
}

/**
 * `HLister` type class instances.
 * 
 * @author Miles Sabin
 */
object HLister {
  implicit def hlister[T <: Product, Out0 <: HList](implicit hlister: HListerAux[T, Out0]) = new HLister[T] {
    type Out = Out0
    def apply(t: T): Out = hlister(t)
  }
}

object HListerAux extends HListerAuxInstances

/**
 * Higher ranked function which converts `Tuples` to `HLists`. 
 */
@deprecated("Use productElements instead", "2.0.0")
object hlisted extends Poly1 {
  implicit def caseProduct[T <: Product](implicit hlister: HLister[T]) = at[T](hlister(_))
}

