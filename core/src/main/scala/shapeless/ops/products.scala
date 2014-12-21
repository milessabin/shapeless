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

import hlist.Length

object product {
  trait ProductLength[T] extends DepFn1[T]

  object ProductLength {
    def apply[T](implicit length: ProductLength[T]): Aux[T, length.Out] = length

    type Aux[T, Out0] = ProductLength[T] { type Out = Out0 }
    
    implicit def length[T, L <: HList]
      (implicit gen: Generic.Aux[T, L], length: Length[L]): Aux[T, length.Out] =
        new ProductLength[T] {
          type Out = length.Out
          def apply(t: T): Out = length()
        }
  }
  
  trait ToHList[P] extends DepFn1[P] { type Out <: HList }
  
  object ToHList {
    def apply[P](implicit toHList: ToHList[P]): Aux[P, toHList.Out] = toHList
    
    type Aux[P, Out0 <: HList] = ToHList[P] { type Out = Out0 }
    
    implicit def toHList[P, Out0 <: HList, L <: HList](implicit
      gen: Generic.Aux[P, L],
      ev: L <:< Out0                              
    ): Aux[P, Out0] =
      new ToHList[P] {
        type Out = Out0
        def apply(p: P) = ev(gen.to(p))
      }
  }
}
