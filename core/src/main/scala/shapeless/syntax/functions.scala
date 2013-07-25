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
package syntax

/**
 * Conversions between ordinary functions and `HList` functions.
 * 
 * The implicits defined by this object enhance ordinary functions (resp. HList functions) with an `hlisted` (resp.
 * `unhlisted`) method which creates an equivalently typed `HList` function (resp. ordinary function).
 * 
 * @author Miles Sabin
 */
object function {
  import ops.function._

  implicit def fnHListOps[F](t : F)(implicit fnHLister : FnHLister[F]) = new FnHListOps[fnHLister.Out] {
    def hlisted = fnHLister(t)
  }


  implicit def fnUnHListOps[F](t : F)(implicit fnUnHLister : FnUnHLister[F]) = new FnUnHListOps[fnUnHLister.Out] {
    def unhlisted = fnUnHLister(t)
  }
}

trait FnHListOps[HLFn] {
  def hlisted : HLFn
}

trait FnUnHListOps[F] {
  def unhlisted : F
}
