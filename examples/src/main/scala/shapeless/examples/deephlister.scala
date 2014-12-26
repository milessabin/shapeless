/*
 * Copyright (c) 2014 Miles Sabin
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

package shapeless.examples

import shapeless._, test._

/*
 * Demo of using Lazy to resolve (hidden) divergence issues when traversing
 * recursive generic structures.
 *
 * See http://stackoverflow.com/questions/25923974
 */
trait DeepHLister[R <: HList] extends DepFn1[R] { type Out <: HList }

trait LowPriorityDeepHLister {
  type Aux[R <: HList, Out0 <: HList] = DeepHLister[R] { type Out = Out0 }

  implicit def headNotCaseClassDeepHLister[H, T <: HList](implicit
    dht: Lazy[DeepHLister[T]]
  ): Aux[H :: T, H :: dht.value.Out] = new DeepHLister[H :: T] {
    type Out = H :: dht.value.Out
    def apply(r: H :: T) = r.head :: dht.value(r.tail)
  }
}

object DeepHLister extends LowPriorityDeepHLister {
  implicit object hnilDeepHLister extends DeepHLister[HNil] {
    type Out = HNil
    def apply(r: HNil) = HNil
  }

  implicit def headCaseClassDeepHLister[H, R <: HList, T <: HList](implicit
    gen: Generic.Aux[H, R],
    dhh: Lazy[DeepHLister[R]],
    dht: Lazy[DeepHLister[T]]
  ): Aux[H :: T, dhh.value.Out :: dht.value.Out] = new DeepHLister[H :: T] {
    type Out = dhh.value.Out :: dht.value.Out
    def apply(r: H :: T) = dhh.value(gen.to(r.head)) :: dht.value(r.tail)
  }

  def apply[R <: HList](implicit dh: DeepHLister[R]): Aux[R, dh.Out] = dh
}

object DeepHListerDemo extends App {
  case class A(x: Int, y: String)
  case class B(x: A, y: A)
  case class C(b: B, a: A)
  case class D(a: A, b: B)

  type ARepr = Int :: String :: HNil
  type BRepr = ARepr :: ARepr :: HNil
  type CRepr = BRepr :: ARepr :: HNil
  type DRepr = ARepr :: BRepr :: HNil

  val adhl = DeepHLister[A :: HNil]
  typed[DeepHLister.Aux[A :: HNil, ARepr :: HNil]](adhl)

  val bdhl = DeepHLister[B :: HNil]
  typed[DeepHLister.Aux[B :: HNil, BRepr :: HNil]](bdhl)

  val cdhl = DeepHLister[C :: HNil]
  typed[DeepHLister.Aux[C :: HNil, CRepr :: HNil]](cdhl)

  val ddhl = DeepHLister[D :: HNil]
  typed[DeepHLister.Aux[D :: HNil, DRepr :: HNil]](ddhl)
}
