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

package shapeless.examples

import shapeless._

object Lift extends App {
  import poly._
  import ops.function._
  import ops.hlist._
  import syntax.std.function._

  /**
   * Lifts a function of arbitrary arity into `Option`. 
   */
  def liftO[InF, InL <: HList, R, OInL <: HList, OutF](f :  InF)
    (implicit
      fntop  : FnToProduct.Aux[InF, InL => R],
      mapped : Mapped.Aux[InL, Option, OInL],
      mapper : Mapper.Aux[get.type, OInL, InL],
      folder : MapFolder[OInL, Boolean, isDefined.type],
      fnfromp: FnFromProduct.Aux[OInL => Option[R], OutF]
    ) : OutF = {
      (o : OInL) =>
        if(o.foldMap(true)(isDefined)(_ && _)) Some(f.toProduct(o map get))
        else None
    }.fromProduct

  object isDefined extends (Option ~>> Boolean) {
    def apply[T](o : Option[T]) = o.isDefined
  }

  object get extends (Option ~> Id) {
    def apply[T](o : Option[T]) = o.get
  }
}
