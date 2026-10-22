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
 * Passes `ClassTag[Nothing]` explicitly where Scala 2 synthesizes it as the sole implicit argument, which Scala 3
 * doesn't. It's shapeless' `ArrayOfNothing.classTag`, which creates arrays as Scala 3 erases them (of
 * `scala.runtime.Nothing$`), rather than `ClassTag.Nothing`, which on Scala 3.3 creates arrays as Scala 2 erases them,
 * {{{
 * Array.empty[Nothing]  // Array.empty[Nothing](using shapeless.ops.ArrayOfNothing.classTag)
 * }}}
 */
class NothingClassTags extends SemanticRule("NothingClassTags") {
  val nothingClassTag: Symbol = Symbol("scala/reflect/ClassTag.Nothing.")

  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case t: Term if t.synthetics.exists(isNothingClassTagArgument) =>
        Patch.addRight(t, "(using shapeless.ops.ArrayOfNothing.classTag)")
    }.asPatch

  val classTag: Symbol = Symbol("scala/reflect/ClassTag#")
  val nothing: Symbol = Symbol("scala/Nothing#")

  def isNothingClassTagArgument(tree: SemanticTree): Boolean = tree match {
    case ApplyTree(OriginalTree(_), List(arg)) => isNothingClassTag(arg)
    case _ => false
  }

  /** Whether `arg` is `ClassTag.Nothing`, or the expansion of the macro materializing a `ClassTag[Nothing]` */
  def isNothingClassTag(arg: SemanticTree): Boolean = arg match {
    case SelectTree(_, id) => id.symbol == nothingClassTag
    case IdTree(info) => info.symbol == nothingClassTag
    case MacroExpansionTree(_, TypeRef(_, `classTag`, List(TypeRef(_, `nothing`, Nil)))) => true
    case _ => false
  }
}
