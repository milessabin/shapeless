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
 * Gives implicit definitions, and vals which override others, without result types the types Scala 2 infers for them.
 *
 * Scala 3 requires implicit definitions to have result types, and gives an overriding val without one the type of the
 * val it overrides, rather than that of its right hand side.
 *
 * The types are written as Scala 2 infers them, so eg. the `Case` of a `Poly` defined by `at` is written as a
 * `Case.Aux`, and that of a `Poly0` as a refinement,
 * {{{
 * implicit def caseInt = at[Int](_.toString)  // implicit def caseInt: Case.Aux[Int, String] = at[Int](_.toString)
 * implicit val zeroInt = at(0)                // implicit val zeroInt: ProductCase[HNil] { type Result = Int } = at(0)
 * }}}
 */
class ExplicitImplicitTypes extends SemanticRule("ExplicitImplicitTypes") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case d: Defn.Def if isImplicit(d.mods) && d.decltpe.isEmpty =>
        ResultTypes.annotate(d, d.name, d.body)
      case v: Defn.Val if v.decltpe.isEmpty =>
        v.pats match {
          case List(p: Pat.Var) if isImplicit(v.mods) || isOverriding(p.name.symbol) =>
            ResultTypes.annotate(v, p.name, v.rhs)
          case _ => Patch.empty
        }
    }.asPatch
  }

  def isImplicit(mods: List[Mod]): Boolean = mods.exists(_.is[Mod.Implicit])

  def isOverriding(sym: Symbol)(implicit doc: SemanticDocument): Boolean =
    sym.info.exists(_.overriddenSymbols.nonEmpty)
}
