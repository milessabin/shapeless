/*
 * Copyright (c) 2011 Miles Sabin 
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
package examples

/**
 * Type level selection sort
 * 
 * @author Miles Sabin
 */
object Sorting {
  import Nat._
  import HList._
  import LT._
  import LTEq._
  
  def typed[T](t : => T) {}

  /**
   * Witness that an HList of Nats is in non-decreasing order at both type and value level.
   */
  trait NonDecreasing[L <: HList]
  implicit def hnilNonDecreasing = new NonDecreasing[HNil] {}
  implicit def hlistNonDecreasing1[H] = new NonDecreasing[H :: HNil] {}
  implicit def hlistNonDecreasing2[H1 <: Nat, H2 <: Nat, T <: HList]
    (implicit ltEq : H1 <= H2, ndt : NonDecreasing[H2 :: T]) = new NonDecreasing[H1 :: H2 :: T] {}
      
  def acceptNonDecreasing[L <: HList](l : L)(implicit ni : NonDecreasing[L]) = l
  
  // Verify type-level relations
  implicitly[NonDecreasing[_1 :: _2 :: _3 :: HNil]]   // OK
  //implicitly[NonDecreasing[_1 :: _3 :: _2 :: HNil]]   // Does not compile
      
  // Apply at the value-level
  acceptNonDecreasing(_1 :: _2 :: _3 :: HNil)         // OK
  //acceptNonDecreasing(_1 :: _3 :: _2 :: HNil)         // Does not compile  

  /**
   * Type class extracting the least element from an HList of Nats at both type and value level.
   * Returns the least element and the remainder in it's original order.
   */
  trait SelectLeast[L <: HList, M <: Nat, Rem <: HList] {
    def apply(l : L) : (M, Rem)
  }
  
  trait LowPrioritySelectLeast {
    implicit def hlistSelectLeast1[H <: Nat, T <: HList] = new SelectLeast[H :: T, H, T] {
      def apply(l : H :: T) : (H, T) = (l.head, l.tail)
    }
  }
  
  object SelectLeast extends LowPrioritySelectLeast {
    implicit def hlistSelectLeast3[H <: Nat, T <: HList, TM <: Nat, TRem <: HList]
      (implicit tsl : SelectLeast[T, TM, TRem], ev : TM < H) = new SelectLeast[H :: T, TM, H :: TRem] {
      def apply(l : H :: T) : (TM, H :: TRem) = {
        val (tm, rem) = tsl(l.tail) 
        (tm, l.head :: rem)
      }
    }
  }
  
  def selectLeast[L <: HList, M <: Nat, Rem <: HList](l : L)(implicit sl : SelectLeast[L, M, Rem]) = sl(l)
  
  val (l1, r1) = selectLeast(_1 :: _2 :: _3 :: HNil)
  typed[_1](l1)
  typed[_2 :: _3 :: HNil](r1)
  
  val (l2, r2) = selectLeast(_3 :: _1 :: _4 :: _0 :: _2 :: HNil)
  typed[_0](l2)
  typed[_3 :: _1 :: _4 :: _2 :: HNil](r2)
  
  /**
   * Type class performing selection sort on an HList of Nats at both the type and value level. 
   */
  trait SelectionSort[L <: HList, S <: HList] {
    def apply(l : L) : S
  }
  
  trait LowPrioritySelectionSort {
    implicit def hlistSelectionSort1[S <: HList] = new SelectionSort[S, S] {
      def apply(l : S) : S = l
    }
  }
  
  object SelectionSort extends LowPrioritySelectionSort {
    implicit def hlistSelectionSort2[L <: HList, M <: Nat, Rem <: HList, ST <: HList]
      (implicit sl : SelectLeast[L, M, Rem], sr : SelectionSort[Rem, ST]) = new SelectionSort[L, M :: ST] {
      def apply(l : L) = {
        val (m, rem) = sl(l)
        m :: sr(rem)
      }
    }
  }
  
  def selectionSort[L <: HList, S <: HList](l : L)(implicit sort : SelectionSort[L, S]) = sort(l)

  /**
   * The punchline ... 
   */
  val unsorted = _3 :: _1 :: _4 :: _0 :: _2 :: HNil
  typed[_3 :: _1 :: _4 :: _0 :: _2 :: HNil](unsorted)
  //acceptNonDecreasing(unsorted)  // Does not compile!
  
  val sorted = selectionSort(unsorted)
  typed[_0 :: _1 :: _2 :: _3 :: _4 :: HNil](sorted)
  acceptNonDecreasing(sorted)    // Compiles!     
}
