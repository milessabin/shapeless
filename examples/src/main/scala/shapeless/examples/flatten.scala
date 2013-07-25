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

package shapeless.examples

/**
 * Flattening arbitrarily nested tuples.
 *
 * @author Miles Sabin
 */
object FlattenExample {
  import shapeless._
  import syntax.tuple._
  import TypeOperators._
  
  def typed[T](t : => T) {}

  trait Flatten[T <: HList] {
    type Out
    def apply(t : T) : Out
  }
  
  object Flatten {
    implicit def flatten[T <: HList, L <: HList](implicit flatten : FlattenAux[T, L]) = new Flatten[T] {
      type Out = L
      def apply(t : T) = flatten(t)
    }
  }
  
  trait FlattenAux[T <: HList, L <: HList] {
    def apply(t : T) : L
  }
  
  trait LowPriorityFlattenAux {
    implicit def flattenHList1[H, T <: HList, OutT <: HList](implicit ft : FlattenAux[T, OutT]) =
      new FlattenAux[H :: T, H :: OutT] {
        def apply(l : H :: T) = l.head :: ft(l.tail)
      }
  }
  
  object FlattenAux extends LowPriorityFlattenAux {
    implicit def flattenHNil = new FlattenAux[HNil, HNil] {
      def apply(t : HNil) = t
    }
    
    implicit def flattenHList2[H <: Product, LH <: HList, T <: HList, OutH <: HList, OutT <: HList, Out <: HList]
      (implicit
        hl : HListerAux[H, LH],
        fh : FlattenAux[LH, OutH],
        ft : FlattenAux[T, OutT],
        prepend : PrependAux[OutH, OutT, Out]
      ) = new FlattenAux[H :: T, Out] {
        def apply(l : H :: T) : Out = fh(hl(l.head)) ::: ft(l.tail)
      }
  }  

  def flatten[T <: Product, L <: HList](t : T)
    (implicit hl : HListerAux[T, L], flatten : Flatten[L]) : flatten.Out = flatten(hl(t))
    
  val t1 = (1, ((2, 3), 4))
  val f1 = flatten(t1)     // Inferred type is Int :: Int :: Int :: Int :: HNil
  val l1 = f1.toList       // Inferred type is List[Int]
  typed[List[Int]](l1)
  
  object toDouble extends Poly1 {
    implicit def caseInt = at[Int](_.toDouble)
    implicit def caseDouble = at[Double](identity)
  }
  
  val t2 = (1, ((2, 3.0), 4))
  val f2 = flatten(t2)     // Inferred type is Int :: Int :: Double :: Int :: HNil
  val ds = f2 map toDouble // Inferred type is Double :: Double :: Double :: Double :: HNil
  val l2 = ds.toList       // Inferred type is List[Double]
  typed[List[Double]](l2)
  
  val t3 = (23, ((true, 2.0, "foo"), "bar"), (13, false))
  val f3 = flatten(t3)     // Inferred type is Int :: Boolean :: Double :: String :: String :: Int :: Boolean :: HNil
  val t3b = f3.tupled      // Inferred type is (Int, Boolean, Double, String, String, Int, Boolean)
  typed[(Int, Boolean, Double, String, String, Int, Boolean)](t3b)
}
