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
 * Replaces uses of what case class companions provide on Scala 2 but not on Scala 3: their `FunctionN` methods, and
 * `unapply` methods yielding `Option`s of tuples of their fields,
 * {{{
 * Foo.tupled         // (Foo.apply _).tupled
 * Foo.unapply(foo)   // Some((foo.a, foo.b))
 * }}}
 */
class CaseClassCompanions extends SemanticRule("CaseClassCompanions") {
  val FunctionN = """scala/Function\d+#""".r

  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case t @ Term.Select(companion: Term.Ref, name) if Set("tupled", "curried")(name.value) && isFunctionMember(name) =>
        Patch.replaceTree(t, s"(${companion.syntax}.apply _).${name.value}")
      case t @ Term.Apply.After_4_6_0(Term.Select(companion: Term.Name, unapply), Term.ArgClause(List(arg), None))
          if unapply.value == "unapply" =>
        fields(unapply.symbol).orElse(localFields(companion)).fold(Patch.empty) { fs =>
          val tuple = fs match {
            case List(f) => s"p.$f"
            case _ => fs.map(f => s"p.$f").mkString("(", ", ", ")")
          }
          val replacement = arg match {
            case n: Term.Name => s"Some(${tuple.replace("p.", s"${n.value}.")})"
            case _ => s"Some(${arg.syntax}).map(p => $tuple)"
          }
          Patch.replaceTree(t, replacement)
        }
    }.asPatch

  def isFunctionMember(name: Term.Name)(implicit doc: SemanticDocument): Boolean =
    FunctionN.matches(name.symbol.owner.value)

  /**
   * The fields of a case class `companion` names which is defined in the document (eg. locally, for which SemanticDB
   * doesn't record the type of the parameter of `unapply`)
   */
  def localFields(companion: Term.Name)(implicit doc: SemanticDocument): Option[List[String]] =
    doc.tree.collect {
      case c: Defn.Class if c.mods.exists(_.is[Mod.Case]) && c.name.value == companion.value =>
        c.ctor.paramClauses.headOption.map(_.values.map(_.name.value))
    }.flatten.headOption

  /** The fields of the case class whose companion's `unapply` method is `unapply` */
  def fields(unapply: Symbol)(implicit doc: SemanticDocument): Option[List[String]] = {
    val cls = unapply.info.map(_.signature).collect {
      case MethodSignature(_, List(List(param)), _) => param.signature
    }.collect {
      case ValueSignature(TypeRef(_, cls, _)) => cls
    }
    cls.filter(_.info.exists(_.isCase)).flatMap(_.info).map(_.signature).collect {
      case ClassSignature(_, _, _, decls) => decls
    }.flatMap { decls =>
      decls.find(_.isPrimary).map(_.signature).collect {
        case MethodSignature(_, params :: _, _) => params.map(_.displayName)
      }
    }
  }
}
