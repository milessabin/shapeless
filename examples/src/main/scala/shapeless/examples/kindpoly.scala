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

import shapeless._

class TyApp[App, TyCon, Args <: HList]

object TyApp {
  class Arbitrary
  implicit def tyApp1[TC[_], A] =
    new TyApp[TC[A], TC[Arbitrary], A :: HNil]
  
  implicit def tyApp2[TC[_, _], A, B] =
    new TyApp[TC[A, B], TC[Arbitrary, Arbitrary], A :: B :: HNil]

  implicit def tyApp3[TC[_, _, _], A, B, C] =
    new TyApp[TC[A, B, C], TC[Arbitrary, Arbitrary, Arbitrary], A :: B :: C :: HNil]
}

object KindPolyExamples {

  def sameTypeArgs[T, ET, U, EU, A <: HList](t: T, u: U)(implicit evt: TyApp[T, ET, A], evu: TyApp[U, EU, A]) = {}

  sameTypeArgs(List(23), Set(13))
  sameTypeArgs(Map(23 -> "foo"), (13, "bar"))
  sameTypeArgs(Map(23 -> "foo"), (x : Int) => "foo")
  // sameTypeArgs(23, "foo")                          // Does not compile
  // sameTypeArgs(List(23), List("foo"))              // Does not compile
  // sameTypeArgs(Map(23 -> "foo"), Map("bar" -> 13)) // Does not compile

  def sameTypeCtor[T, U, TC](t: T, u: U)(implicit evt: TyApp[T, TC, _], evu: TyApp[U, TC, _]) = {}

  sameTypeCtor(List(23), List("foo"))
  sameTypeCtor(Map(23 -> "foo"), Map(true -> ()))
  sameTypeCtor((x : Int, y: Int) => Boolean, (x : Double, y : Double) => Boolean) 
  // sameTypeCtor(23, true)                           // Does not compile
  // sameTypeCtor(List(23), Set(13))                  // Does not compile
  // sameTypeCtor(Map(23 -> "foo"), (13, "bar"))      // Does not compile
}
