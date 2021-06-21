/*
 * Copyright (c) 2011-16 Miles Sabin
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

trait RecordScalaCompat {
  def applyDynamic(method: String)(rec: Any*): HList = macro RecordMacros.mkRecordEmptyImpl
  def applyDynamicNamed(method: String)(rec: Any*): HList = macro RecordMacros.mkRecordNamedImpl
}

class RecordMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  import internal.constantType

  val hconsValueTree: Tree = reify(::).tree
  val hnilValueTree: Tree = reify(HNil: HNil).tree

  def mkRecordEmptyImpl(method: Tree)(rec: Tree*): Tree = {
    if (rec.nonEmpty) abort("this method must be called with named arguments")
    hnilValueTree
  }

  def mkRecordNamedImpl(method: Tree)(rec: Tree*): Tree = {
    val q"${methodString: String}" = (method: @unchecked)
    if (methodString != "apply") abort(s"this method must be called as 'apply' not '$methodString'")
    mkRecordImpl(rec: _*)
  }

  def mkRecordImpl(rec: Tree*): Tree = {
    def mkElem(key: Type, value: Tree): Tree =
      q"$value.asInstanceOf[${FieldType(key, value.tpe.widen)}]"

    def promoteElem(elem: Tree): Tree = elem match {
      case q"$_(${Literal(k)}, $v)" => mkElem(constantType(k), v)
      case _ => abort(s"$elem has the wrong shape for a record field")
    }

    rec.foldRight(hnilValueTree) { (elem, acc) =>
      q"$hconsValueTree(${promoteElem(elem)}, $acc)"
    }
  }
}
