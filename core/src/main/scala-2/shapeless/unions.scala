/*
 * Copyright (c) 2013-16 Miles Sabin
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

package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait UnionScalaCompat {
  def applyDynamicNamed[U <: Coproduct](method: String)(elems: Any*): U = macro UnionMacros.mkUnionNamedImpl[U]
  def selectDynamic(tpeSelector: String): Any = macro LabelledMacros.unionType
}

class UnionMacros(val c: whitebox.Context) {
  import c.universe._
  import internal.constantType
  import labelled.FieldType

  def mkUnionNamedImpl[U <: Coproduct : WeakTypeTag](method: Tree)(elems: Tree*): Tree = {
    val fieldTypeTpe = typeOf[FieldType[_, _]].typeConstructor
    val coproduct = reify(Coproduct)

    def mkFieldTpe(keyTpe: Type, valueTpe: Type): Type =
      appliedType(fieldTypeTpe, List(keyTpe, valueTpe.widen))

    def mkElem(keyTpe: Type, value: Tree): Tree =
      q"$value.asInstanceOf[${mkFieldTpe(keyTpe, value.tpe)}]"

    def promoteElem(elem: Tree): Tree = elem match {
      case q"$_(${Literal(k)}, $v)" => mkElem(constantType(k), v)
      case _ => c.abort(c.enclosingPosition, s"$elem has the wrong shape for a record field")
    }

    val q"${methodString: String}" = (method: @unchecked)
    if (methodString != "apply")
      c.abort(c.enclosingPosition, s"this method must be called as 'apply' not '$methodString'")

    val elem = elems match {
      case Seq(e) => e
      case _ => c.abort(c.enclosingPosition, "only one branch of a union may be inhabited")
    }

    q"$coproduct[${weakTypeOf[U]}](${promoteElem(elem)})"
  }
}
