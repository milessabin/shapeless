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
 * Gives `Sized(...)` the collection type which Scala 2 infers for it (by default `IndexedSeq`), which Scala 3 doesn't,
 * {{{
 * Sized(1, 2)  // Sized[IndexedSeq](1, 2)
 * }}}
 */
class ExplicitSizedCollection extends SemanticRule("ExplicitSizedCollection") {
  val sized: Symbol = Symbol("shapeless/Sized.")
  val sizedApply: Symbol = Symbol("shapeless/Sized.apply().")

  override def fix(implicit doc: SemanticDocument): Patch = {
    val printer = new TypePrinter
    doc.tree.collect {
      case Term.Apply.After_4_6_0(fn: Term.Ref, _) if fn.symbol == sized =>
        val collection = fn.synthetics.collectFirst {
          case TypeApplyTree(SelectTree(_, id), List(cc)) if id.symbol == sizedApply => cc
        }
        collection.flatMap(printer.print(_, Symbol.None)) match {
          case Some(cc) => Patch.addRight(fn, s"[$cc]")
          case None => Patch.empty
        }
    }.asPatch
  }
}
