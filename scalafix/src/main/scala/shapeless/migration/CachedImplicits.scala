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
 * Initializes implicit vals without result types which summon their values (eg. `implicit val m = Monoid[Foo]`) with
 * `cachedImplicit` instead.
 *
 * On Scala 2 an implicit val without a result type isn't a candidate for the implicit search which initializes it, but
 * on Scala 3 it requires one (see `ExplicitImplicitTypes`), with which it would be, and would initialize itself.
 * `cachedImplicit` excludes the val it initializes from the search,
 * {{{
 * implicit val m = Monoid[Foo]  // implicit val m: Monoid[Foo] = cachedImplicit
 * }}}
 */
class CachedImplicits extends SemanticRule("CachedImplicits") {
  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case v: Defn.Val if v.mods.exists(_.is[Mod.Implicit]) && v.decltpe.isEmpty && isSummon(v.rhs) =>
        Patch.replaceTree(v.rhs, cachedImplicit)
    }.asPatch

  /** Whether `rhs` applies a method whose only parameters are implicit to no arguments */
  def isSummon(rhs: Term)(implicit doc: SemanticDocument): Boolean = rhs match {
    case Term.ApplyType.After_4_6_0(fn, _) => isSummoner(method(fn))
    case fn: Term.Ref => isSummoner(method(fn))
    case _ => false
  }

  /** The method `fn` refers to, which is the `apply` method of the object it refers to, if any */
  def method(fn: Term)(implicit doc: SemanticDocument): Symbol =
    fn.synthetics.collectFirst { case SelectTree(_, id) if id.symbol.displayName == "apply" => id.symbol }
      .getOrElse(fn.symbol)

  def isSummoner(sym: Symbol)(implicit doc: SemanticDocument): Boolean =
    sym.info.map(_.signature).exists {
      case MethodSignature(_, List(params), _) => params.nonEmpty && params.forall(_.isImplicit)
      case _ => false
    }

  /** `cachedImplicit`, qualified where it isn't visible */
  def cachedImplicit(implicit doc: SemanticDocument): String = {
    val printer = new TypePrinter
    val shapeless = Symbol("shapeless/")
    if (printer.packages(shapeless) || printer.wildcardImports(shapeless) || printer.namedImports.contains("cachedImplicit"))
      "cachedImplicit"
    else "shapeless.cachedImplicit"
  }
}
