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

trait HListScalaCompat {

  /**
   * Allows to specify an `HList` type with a syntax similar to `Record` and `Union`, as follows,
   *
   * {{{
   * type ISB = HList.`Int, String, Boolean`.T
   * }}}
   *
   * Literal types are allowed, so that the following is valid,
   *
   * {{{
   * type ABC = HList.`'a, 'b, 'c`.T
   * type TwoTrueStr = HList.`2, true, "str"`.T
   * }}}
   */
  def selectDynamic(tpeSelector: String): Any = ???
}

trait NatProductArgsScalaCompat {
  def applyDynamic(method: String)(args: Int*): Any = ???
}

trait ProductArgsScalaCompat {
  def applyDynamic(method: String)(args: Any*): Any = ???
}

trait FromProductArgsScalaCompat {
  def applyDynamic(method: String)(hlist: HList): Any = ???
}

trait SingletonProductArgsScalaCompat {
  def applyDynamic(method: String)(args: Any*): Any = ???
}
