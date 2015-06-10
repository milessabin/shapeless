/*
 * Copyright (c) 2013-14 Miles Sabin 
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

import scala.language.dynamics
import scala.language.experimental.macros

sealed trait Coproduct

sealed trait :+:[+H, +T <: Coproduct] extends Coproduct

final case class Inl[+H, +T <: Coproduct](head : H) extends :+:[H, T] {
  override def toString = head.toString
}

final case class Inr[+H, +T <: Coproduct](tail : T) extends :+:[H, T] {
  override def toString = tail.toString
}

sealed trait CNil extends Coproduct

object Coproduct extends Dynamic {
  import ops.coproduct.Inject
  import syntax.CoproductOps

  class MkCoproduct[C <: Coproduct] {
    def apply[T](t: T)(implicit inj: Inject[C, T]): C = inj(t) 
  }
  
  def apply[C <: Coproduct] = new MkCoproduct[C]

  implicit def cpOps[C <: Coproduct](c: C) = new CoproductOps(c) 

  def unsafeMakeCoproduct(length: Int, value: Any) = (0 until length).foldLeft[Coproduct](Inl(value))((accum, _) => Inr(accum))
  def unsafeGet(c: Coproduct): Any = c match {
    case Inl(h) => h
    case Inr(c) => unsafeGet(c)
  }


  /**
   * Allows to specify a `Coproduct` type with a syntax similar to `Record` and `Union`, as follows,
   *
   * {{{
   * type ISB = Coproduct.`Int, String, Boolean`.T
   * }}}
   *
   * Literal types are allowed, so that the following is valid,
   *
   * {{{
   * type ABC = Coproduct.`'a, 'b, 'c`.T
   * type TwoTrueStr = Coproduct.`2, true, "str"`.T
   * }}}
   */
  def selectDynamic(tpeSelector: String): Any = macro LabelledMacros.coproductTypeImpl
}
