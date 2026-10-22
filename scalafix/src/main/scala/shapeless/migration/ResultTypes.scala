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

/** Writing the result types which Scala 2 infers for definitions */
object ResultTypes {
  /** Writes the result type of `defn`, defining `name` as `body`, between its signature and `=` */
  def annotate(defn: Tree, name: Term.Name, body: Term)(implicit doc: SemanticDocument): Patch = {
    val printer = new TypePrinter(enclosingTemplates(defn).toSet)
    val printed = resultType(name.symbol).flatMap(printer.print(_, enclosingTemplate(defn)))
    val signature = defn.tokens.takeWhile(_.start < body.pos.start).toList
    val equals = signature.lastIndexWhere(_.is[Token.Equals])
    val end = signature.lastIndexWhere(t => !t.text.forall(_.isWhitespace), equals - 1)
    printed match {
      case Some(tpe) if equals >= 0 && end >= 0 =>
        Patch.removeTokens(signature.slice(end + 1, equals)) + Patch.addRight(signature(end), s": $tpe ")
      case _ =>
        Patch.lint(Diagnostic("", s"The inferred type of ${name.value} can't be written explicitly", name.pos))
    }
  }

  def resultType(sym: Symbol)(implicit doc: SemanticDocument): Option[SemanticType] =
    sym.info.map(_.signature).collect {
      case MethodSignature(_, _, tpe) => tpe
      case ValueSignature(tpe) => tpe
    }

  /** The classes and objects enclosing `tree` */
  def enclosingTemplates(tree: Tree)(implicit doc: SemanticDocument): List[Symbol] =
    tree.parent.toList.flatMap { parent =>
      val template = parent match {
        case o: Defn.Object => List(o.name.symbol)
        case c: Defn.Class => List(c.name.symbol)
        case t: Defn.Trait => List(t.name.symbol)
        case _ => Nil
      }
      template ++ enclosingTemplates(parent)
    }

  /** The class or object whose `this` a definition sees */
  def enclosingTemplate(tree: Tree)(implicit doc: SemanticDocument): Symbol =
    tree.parent.fold(Symbol.None) {
      case o: Defn.Object => o.name.symbol
      case c: Defn.Class => c.name.symbol
      case t: Defn.Trait => t.name.symbol
      case _: Term.NewAnonymous => Symbol.None
      case parent => enclosingTemplate(parent)
    }
}
