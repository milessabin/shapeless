/*
 * Copyright (c) 2011-16 Miles Sabin
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

/**
 * Record operations on `HList`'s with field-like elements.
 *
 * @author Miles Sabin
 */
object record {
  import syntax.RecordOps

  implicit def recordOps[L <: HList](l : L): RecordOps[L] = new RecordOps(l)

  /**
   * Records encoded as `HLists` of their value types intersected with the
   * singleton types of their keys.
   *
   * Record types may be written using a relatively concise syntax thanks to a trick
   * due to Denys Shabalin (@den_sh) and Eugene Burmako (@xeno_by). We use a
   * combination of `selectDynamic` and backticks to embed a type in a path which
   * appears to the compiler as stable,
   *
   * {{{
   * type Xyz = Record.`"x" -> Int, "y" -> String, "z" -> Boolean`.T
   * }}}
   *
   * The use of singleton-typed `Symbols` as keys would make this type extremely
   * laborious to write out by hand.
   *
   * There is also a mechanism for creating values of record types using Scala's
   * named argument syntax. Values of the type just defined can be created as follows,
   *
   * {{{
   * val xyz = Record(x = 23, y = "foo", z = true)
   * xyz("y") // == "foo"
   * }}}
   */
  object Record extends Dynamic with RecordScalaCompat
}
