/*
 * Copyright (c) 2026 Miles Sabin
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

package shapeless.migration

import scala.meta._
import scalafix.v1._

/**
 * Marks type parameters which are arguments of the types of type patterns as `@unchecked`, which Scala 3 otherwise
 * warns can't be checked,
 * {{{
 * case c: Cons[T] => ...  // case c: Cons[T @unchecked] => ...
 * }}}
 */
class UncheckedTypeArguments extends SemanticRule("UncheckedTypeArguments") {
  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case Pat.Typed(_, Type.Apply.After_4_6_0(_, args)) =>
        args.values.collect {
          case n: Type.Name if n.symbol.value.endsWith("]") => Patch.addRight(n, " @unchecked")
        }.asPatch
    }.asPatch
}
