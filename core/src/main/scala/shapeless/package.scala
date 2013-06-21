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

package object shapeless {
  import Poly._
  
  /** The SYB everything combinator */
  type Everything[F <: Poly, K <: Poly, T] = Case1Aux[EverythingAux[F, K], T]
  
  class ApplyEverything[F <: Poly] {
    def apply(k : Poly) = new EverythingAux[F, k.type]
  }
  
  def everything(f: Poly) = new ApplyEverything[f.type]

  /** The SYB everywhere combinator */
  type Everywhere[F <: Poly, T] = Case1Aux[EverywhereAux[F], T]

  def everywhere(f: Poly) = new EverywhereAux[f.type]
}
