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

object function {
  /**
   * Type class supporting conversion of arbitrary functions to functions of a single `HList`
   * argument. 
   * 
   * @author Miles Sabin
   */
  trait FnHLister[F] {
    type Out = Args => Result
    type Args <: HList
    type Result
    
    def apply(f : F) : Out
  }
    
  trait FnHListerAux[F, Out] {
    type Args <: HList
    type Result
    def apply(f : F) : Out
  }

  /**
   * `FnHLister` type class instances.
   * 
   * @author Miles Sabin
   */
  object FnHLister {
    implicit def fnHLister[F, Args0 <: HList, Result0](implicit fnHLister : FnHListerAux[F, Args0 => Result0]) = new FnHLister[F] {
      type Args = Args0
      type Result = Result0
      def apply(f : F) : Out = fnHLister(f)
    }
  }

  object FnHListerAux extends FnHListerAuxInstances

  /**
   * Type class supporting conversion of functions of a single `HList` argument to ordinary functions. 
   * 
   * @author Miles Sabin
   */
  trait FnUnHLister[F] {
    type Out
    def apply(f : F) : Out
  }
    
  trait FnUnHListerAux[F, Out] {
    def apply(f : F) : Out
  }
    
  /**
   * `FnUnHLister` type class instances.
   * 
   * @author Miles Sabin
   */
  object FnUnHLister {
    implicit def fnUnHLister[F, Out0](implicit fnUnHLister : FnUnHListerAux[F, Out0]) = new FnUnHLister[F] {
      type Out = Out0
      def apply(f : F) : Out = fnUnHLister(f)
    }
  }

  object FnUnHListerAux extends FnUnHListerAuxInstances
}
