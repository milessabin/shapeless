/*
 * Copyright (c) 2011-13 Miles Sabin 
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
 * Conversions between `Tuples` and `HLists`.
 * 
 * The implicit defined by this object enhances `Tuples` with an `hlisted` method which constructs
 * an equivalently typed [[shapeless.HList]]. This object also provides higher ranked functions for
 * conversion between `Tuples` and `HLists`.
 * 
 * @author Miles Sabin
 */
object Tuples {
  trait TupleOps[L <: HList] {
    def hlisted : L
  }
  
  implicit def tupleOps[T <: Product](t : T)(implicit hlister : HLister[T]) = new TupleOps[hlister.Out] {
    def hlisted = hlister(t)
  }
  
  /**
   * Higher ranked function which converts `Tuples` to `HLists`. 
   */
  object hlisted extends Poly1 {
    implicit def caseProduct[T <: Product](implicit hlister : HLister[T]) = at[T](hlister(_))
  }

  /**
   * Higher ranked function which converts `HLists` to `Tuples`. 
   */
  object tupled extends Poly1 {
    implicit def caseHList[L <: HList](implicit tupler : Tupler[L]) = at[L](tupler(_))
  }
}

}

/**
 */
object productElements extends Poly1 {
  implicit def caseProduct[P](implicit gen: Generic[P]) = at[P](p => gen.to(p))
}
