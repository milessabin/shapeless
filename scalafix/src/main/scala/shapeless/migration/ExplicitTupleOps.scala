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
 * Makes explicit the conversion of tuples to shapeless' `TupleOps` where the operation selected has the name of a member
 * of `scala.Tuple`, which on Scala 3 takes precedence over the conversion,
 * {{{
 * t.tail.head  // productTupleOps(productTupleOps(t).tail).head
 * }}}
 */
class ExplicitTupleOps extends SemanticRule("ExplicitTupleOps") {
  /** The members of `scala.Tuple` (as of Scala 3.10) */
  val tupleMembers: Set[String] = Set(
    "*:", ":*", "++", "apply", "drop", "head", "init", "last", "map", "reverse", "size", "splitAt", "tail", "take",
    "toArray", "toIArray", "toList", "zip"
  )

  val conversion: Symbol = Symbol("shapeless/syntax/std/tuple.productTupleOps().")

  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case Term.Select(qual, op) if tupleMembers(op.value) =>
        explicit(qual)
      case t: Term.ApplyInfix if tupleMembers(t.op.value) =>
        (if (t.op.value.endsWith(":")) t.argClause.values else List(t.lhs)).map(explicit).asPatch
    }.asPatch

  def explicit(receiver: Term)(implicit doc: SemanticDocument): Patch =
    if (receiver.synthetics.exists(converts))
      Patch.addLeft(receiver, "productTupleOps(") + Patch.addRight(receiver, ")")
    else Patch.empty

  def converts(tree: SemanticTree): Boolean = tree match {
    case ApplyTree(fn, args) => converts(fn) || args.exists(converts)
    case TypeApplyTree(fn, _) => converts(fn)
    case SelectTree(_, id) => id.symbol == conversion
    case IdTree(info) => info.symbol == conversion
    case _ => false
  }
}
