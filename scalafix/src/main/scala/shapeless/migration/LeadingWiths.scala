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
 * Moves `with`s which end lines of the parents of templates to the start of the following lines, as on Scala 3 a `with`
 * ending a line may start an indented template body,
 * {{{
 * trait A extends        trait A extends
 *   B with                 B
 *   C                      with C
 * }}}
 */
class LeadingWiths extends SyntacticRule("LeadingWiths") {
  override def fix(implicit doc: SyntacticDocument): Patch =
    doc.tree.collect {
      case t: Template if t.inits.nonEmpty =>
        val (start, end) = (t.inits.head.pos.start, t.inits.last.pos.end)
        val tokens = t.tokens.toList.dropWhile(_.start < start).takeWhile(_.end <= end)
        tokens.zipWithIndex.collect {
          case (w, i) if w.is[Token.KwWith] =>
            val (space, rest) = tokens.drop(i + 1).span(_.text.forall(_.isWhitespace))
            val before = tokens.take(i).reverse.takeWhile(_.text.forall(_.isWhitespace))
            rest.headOption.filter(_ => space.exists(_.text.contains('\n'))).fold(Patch.empty) { next =>
              Patch.removeTokens(before) + Patch.removeToken(w) + Patch.addLeft(next, "with ")
            }
        }.asPatch
    }.asPatch
}
