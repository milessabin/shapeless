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

object Lift {
  import PolyFun._
  import HList._
  import Functions._

  def liftO[InF, InL <: HList, R, OInL <: HList, OutF](f :  InF)
    (implicit
      hlister   : FnHListerAux[InF, InL => R],
      mapper    : MapperAux[get.type, OInL, InL],
      folder    : LeftFolder[OInL, Boolean, isDefined.type],
      unhlister : FnUnHListerAux[OInL => Option[R], OutF]
    ) : OutF = {
      (o : OInL) =>
        if(o.foldLeft(true)(isDefined)(_ && _)) Some(f.hlisted(o map get))
        else None
    }.unhlisted
}
