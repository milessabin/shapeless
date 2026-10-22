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
 * Scopes implicit definitions in blocks so that, as on Scala 2, they aren't visible to the statements which precede
 * them, as they otherwise are on Scala 3,
 * {{{
 * {                                {
 *   val a = implicitly[Int]          val a = implicitly[Int]
 *   implicit val i = 23              locally {
 *   val b = implicitly[Int]            implicit val i = 23
 * }                                    val b = implicitly[Int]
 *                                    }
 *                                  }
 * }}}
 * Only the first implicit definition which follows other statements is scoped, and only where the statements from it
 * on define nothing which those before it refer to.
 */
class LocalImplicitScopes extends SemanticRule("LocalImplicitScopes") {
  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case b: Term.Block => scope(b.stats)
    }.asPatch

  def scope(stats: List[Stat])(implicit doc: SemanticDocument): Patch = {
    val i = stats.indexWhere(isImplicitDefinition)
    val (before, after) = stats.splitAt(i)
    if (i <= 0 || before.forall(isImplicitDefinition)) Patch.empty
    else {
      val defined = after.flatMap(definitions).toSet
      val referred = before.flatMap(_.collect { case n: Term.Name => n.symbol })
      if (referred.exists(defined)) Patch.empty else wrap(after)
    }
  }

  def isImplicitDefinition(stat: Stat): Boolean = stat match {
    case d: Defn.Val => d.mods.exists(_.is[Mod.Implicit])
    case d: Defn.Def => d.mods.exists(_.is[Mod.Implicit])
    case d: Defn.Object => d.mods.exists(_.is[Mod.Implicit])
    case d: Defn.Class => d.mods.exists(_.is[Mod.Implicit])
    case _ => false
  }

  def definitions(stat: Stat)(implicit doc: SemanticDocument): List[Symbol] = stat match {
    case d: Defn.Val => d.pats.flatMap(_.collect { case v: Pat.Var => v.name.symbol })
    case d: Defn.Var => d.pats.flatMap(_.collect { case v: Pat.Var => v.name.symbol })
    case d: Defn.Def => List(d.name.symbol)
    case d: Defn.Object => List(d.name.symbol)
    case d: Defn.Class => List(d.name.symbol)
    case d: Defn.Trait => List(d.name.symbol)
    case _ => Nil
  }

  /** Wraps `stats` in `locally`, indenting them further */
  def wrap(stats: List[Stat])(implicit doc: SemanticDocument): Patch = {
    val (first, last) = (stats.head, stats.last)
    val indent = " " * first.pos.startColumn
    val tokens = doc.tree.tokens.toList.dropWhile(_.start < first.pos.start).takeWhile(_.end <= last.pos.end)
    val reindent = tokens.zip(tokens.drop(1)).collect {
      case (nl, next) if isNewline(nl) && !isNewline(next) => Patch.addRight(nl, "  ")
    }
    Patch.addLeft(first.tokens.head, s"locally {\n$indent  ") + reindent.asPatch +
      Patch.addRight(last.tokens.last, s"\n$indent}")
  }

  def isNewline(token: Token): Boolean = token.text.contains('\n') && token.text.forall(_.isWhitespace)
}
