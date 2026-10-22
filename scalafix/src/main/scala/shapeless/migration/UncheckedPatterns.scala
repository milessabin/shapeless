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
 * Marks the right hand sides of pattern definitions with patterns which may be refutable as `@unchecked`, which Scala 3
 * otherwise warns of,
 * {{{
 * val h :: t = l  // val h :: t = (l: @unchecked)
 * }}}
 */
class UncheckedPatterns extends SyntacticRule("UncheckedPatterns") {
  override def fix(implicit doc: SyntacticDocument): Patch =
    doc.tree.collect {
      case v: Defn.Val if v.pats.exists(isRefutable) && !isUnchecked(v.rhs) =>
        Patch.addLeft(v.rhs, "(") + Patch.addRight(v.rhs, ": @unchecked)")
    }.asPatch

  /** Whether `pat` may be refutable: variable and wildcard patterns, and tuples of them, never are */
  def isRefutable(pat: Pat): Boolean = pat match {
    case _: Pat.Var | _: Pat.Wildcard => false
    case Pat.Typed(_: Pat.Var | _: Pat.Wildcard, _) => false
    case t: Pat.Tuple => t.args.exists(isRefutable)
    case _ => true
  }

  def isUnchecked(rhs: Tree): Boolean = rhs match {
    case Term.Ascribe(_, Type.Annotate(_, annots)) => annots.exists(_.init.tpe.syntax == "unchecked")
    case _ => false
  }
}
