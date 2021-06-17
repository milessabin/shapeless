/*
 * Copyright (c) 2013-16 Miles Sabin
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

object union {
  import syntax.UnionOps

  implicit def unionOps[C <: Coproduct](u : C) : UnionOps[C] = new UnionOps(u)

  /**
   * Discriminated unions encoded as `Coproducts` of their value types intersected with
   * the singleton types of their keys.
   *
   * Union types may be written using a relatively concise syntax thanks to a trick
   * due to Denys Shabalin (@den_sh) and Eugene Burmako (@xeno_by). We use a
   * combination of `selectDynamic` and backticks to embed a type in a path which
   * appears to the compiler as stable,
   *
   * {{{
   * type Xyz = Union.`"x" -> Int, "y" -> String, "z" -> Boolean`.T
   * }}}
   *
   * The use of singleton-typed `Symbols` as keys would make this type extremely
   * laborious to write out by hand.
   *
   * There is also a mechanism for creating values of union types using Scala's
   * named argument syntax. Values of the type just defined can be created as follows,
   *
   * {{{
   * val y = Union[Xyz](y = "foo")
   * y.get("y") // == Some("foo")
   * }}}
   */
  object Union extends Dynamic with UnionScalaCompat
}
