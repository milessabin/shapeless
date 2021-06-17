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
package ops
package record

trait SelectorScalaCompat {

  implicit def materialize[R <: HList, K, O]: Selector.Aux[R, K, O] = ???
}

trait UpdaterScalaCompat {

  implicit def meterialize[L <: HList, F, O <: HList]: Updater.Aux[L, F, O] = ???
}

trait ModifierScalaCompat {
  implicit def materialize[R <: HList, K, A, B, O <: HList]: Modifier.Aux[R, K, A, B, O] = ???
}

trait RemoverScalaCompat {
  implicit def materialize[R <: HList, K, V, O <: HList]: Remover.Aux[R, K, (V, O)] = ???
}

trait LacksKeyScalaCompat {
  implicit def materialize[R <: HList, K]: LacksKey[R, K] = ???
}
