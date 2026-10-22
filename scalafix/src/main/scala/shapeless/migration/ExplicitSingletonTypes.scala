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
 * Gives vals without result types, whose types Scala 2 infers to be singleton types, those types, which Scala 3 widens,
 * {{{
 * val s = 23.narrow  // val s: 23 = 23.narrow
 * val w = wO.value   // val w: o.type = wO.value
 * }}}
 */
class ExplicitSingletonTypes extends SemanticRule("ExplicitSingletonTypes") {
  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case v: Defn.Val if v.decltpe.isEmpty && !v.mods.exists(_.is[Mod.Implicit]) =>
        v.pats match {
          case List(p: Pat.Var) if isSingleton(p.name.symbol) => ResultTypes.annotate(v, p.name, v.rhs)
          case _ => Patch.empty
        }
    }.asPatch

  def isSingleton(sym: Symbol)(implicit doc: SemanticDocument): Boolean =
    sym.info.exists(_.overriddenSymbols.isEmpty) && ResultTypes.resultType(sym).exists {
      case _: ConstantType => true
      case SingleType(_, path) => !path.isNone && !isObject(path)
      case _ => false
    }

  /** Whether `sym` is an object, whose singleton type Scala 3 infers too */
  def isObject(sym: Symbol)(implicit doc: SemanticDocument): Boolean = sym.info.exists(_.isObject)
}
