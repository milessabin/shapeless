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
class HListIso[T, L <: HList](ctor : L => T, dtor : T => L) {
  def fromHList(l : L) : T = ctor(l)
  def toHList(t : T) : L = dtor(t)
}

object HListIso {
  import Functions._
  import Tuples._

  def apply[CC, C, T <: Product, L <: HList](apply : C, unapply : CC => Option[T])
    (implicit fhl : FnHListerAux[C, L => CC], hl : HListerAux[T, L]) =
      new HListIso(apply.hlisted, (cc : CC) => hl(unapply(cc).get))
  
  // Special case for one-element cases classes because their unapply result types
  // are Option[T] rather than Option[Tuple1[T]] which would be required to fit
  // the general case.
  def apply[CC, T](apply : T => CC, unapply : CC => Option[T]) =
      new HListIso(apply.hlisted, (cc : CC) => unapply(cc).get :: HNil)

  def fromHList[T, L <: HList](l : L)(implicit iso : HListIso[T, L]) = iso.fromHList(l)
  
  def toHList[T, L <: HList](t : T)(implicit iso : HListIso[T, L]) = iso.toHList(t) 
  
  implicit def tupleIso[T <: Product, L <: HList](implicit hl : HListerAux[T, L], uhl : TuplerAux[L, T]) =
    new HListIso(uhl.apply, hl.apply)
}
