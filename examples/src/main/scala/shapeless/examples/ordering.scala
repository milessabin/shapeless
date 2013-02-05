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

import shapeless.{::, HList, HNil}

object Ordering {

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
