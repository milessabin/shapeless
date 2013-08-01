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

package shapeless.examples

import shapeless.{ Generic, ::, HList, HNil }

object OrderingExamples extends App {

  // Derive an Ordering for an HList from the Orderings of its elements

  trait LowPriorityGenericOrdering {
    // An Ordering for any type which is isomorphic to an HList, if that HList has an Ordering
  
    implicit def hlistIsoOrdering[A, H <: HList](implicit gen : Generic.Aux[A, H], oh : Ordering[H]) : Ordering[A] = new Ordering[A] {
      def compare(a1 : A, a2 : A) = oh.compare(gen to a1, gen to a2)
    }
  }
  
  object GenericOrdering extends LowPriorityGenericOrdering {
    implicit def hnilOrdering : Ordering[HNil] = new Ordering[HNil] {
      def compare(a : HNil, b : HNil) = 0
    }
  
    implicit def hlistOrdering[H, T <: HList](implicit oh : Ordering[H], ot : Ordering[T]) : Ordering[H :: T] = new Ordering[H :: T] {
      def compare(a : H :: T, b : H :: T) = {
        val i = oh.compare(a.head, b.head)
        if (i == 0) ot.compare(a.tail, b.tail)
        else i
      }
    }
  }
  
  import GenericOrdering._

  implicitly[Ordering[Int :: String :: HNil]]
  val hs = List(
    2 :: "b" :: HNil,
    2 :: "a" :: HNil,
    1 :: "c" :: HNil
  ).sorted
  assert(hs == List(
    1 :: "c" :: HNil,
    2 :: "a" :: HNil,
    2 :: "b" :: HNil
  ))

  case class Foo(i : Int, s : String)

  implicitly[Ordering[Foo]]
  val fs = List(
    Foo(2, "b"),
    Foo(2, "a"),
    Foo(1, "c")
  ).sorted
  assert(fs == List(
    Foo(1, "c"),
    Foo(2, "a"),
    Foo(2, "b")
  ))

}
