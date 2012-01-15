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

import org.junit.Test
import org.junit.Assert._

class StackOverflow1 {
  // http://stackoverflow.com/questions/7606587

  import HList._

  trait FoldCurry[L <: HList, F, Out] {
    def apply(l : L, f : F) : Out
  }
  
  implicit def foldCurry1[H, Out] = new FoldCurry[H :: HNil, H => Out, Out] {
    def apply(l : H :: HNil, f : H => Out) = f(l.head)
  }
  
  implicit def foldCurry2[H, T <: HList, FT, Out](implicit fct : FoldCurry[T, FT, Out]) =
    new FoldCurry[H :: T, H => FT, Out] {
      def apply(l : H :: T, f : H => FT) = fct(l.tail, f(l.head))
    }
  
  def foldCurry[L <: HList, F, Out](l : L, f : F)(implicit fc : FoldCurry[L, F, Out]) : Out = fc(l, f)
  
  def typed[T](t : => T) {}

  @Test
  def testFoldCurry {
    val f1 = (i : Int, j : Int, k : Int, l : Int) => i+j+k+l
    val f1c = f1.curried
    
    val l1 = 1 :: 2 :: 3 :: 4 :: HNil

    val r1 = foldCurry(l1, f1c)
    typed[Int](r1)
    assertEquals(10, r1)
    
    val f2 = (i : Int, s : String, d : Double) => (i+1, s.length, d*2)
    val f2c = f2.curried
    
    val l2 = 23 :: "foo" :: 2.0 :: HNil
    
    val r2 = foldCurry(l2, f2c)
    typed[(Int, Int, Double)](r2)
    assertEquals((24, 3, 4.0), r2)
  }
}
