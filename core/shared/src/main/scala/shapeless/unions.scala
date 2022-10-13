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

import scala.language.dynamics
import scala.language.experimental.macros

import scala.reflect.macros.whitebox

object union {
  import syntax.UnionOps

  implicit def unionOps[C <: Coproduct](u : C) : UnionOps[C] = new UnionOps(u)

  /**
   * Discriminated unions encoded as `Coproducts` of their value types intersected with
   * the singleton types of their keys.
   *
   * Union types may be written using a relatively concise syntax thanks to a trick
   * due to Denys Shabalin (@den_sh) and Eugene Burmako (@xeno_by). We use a
   * combination of `selectDynamic` and backticks to embed a type in a path which
   * appears to the compiler as stable,
   *
   * {{{
   * type Xyz = Union.`'x -> Int, 'y -> String, 'z -> Boolean`.T
   * }}}
   *
   * The use of singleton-typed `Symbols` as keys would make this type extremely
   * laborious to write out by hand.
   *
   * There is also a mechanism for creating values of union types using Scala's
   * named argument syntax. Values of the type just defined can be created as follows,
   *
   * {{{
   * val y = Union[Xyz](y = "foo")
   * y.get('y) // == Some("foo")
   * }}}
   */
  object Union extends Dynamic {
    def applyDynamicNamed[U <: Coproduct](method: String)(elems: Any*): U = macro UnionMacros.mkUnionNamedImpl[U]

    def selectDynamic(tpeSelector: String): Any = macro LabelledMacros.unionTypeImpl
  }
}

class UnionMacros(val c: whitebox.Context) {
  import c.universe._
  import internal.constantType
  import labelled.FieldType

  val fieldTypeTpe = typeOf[FieldType[_, _]].typeConstructor
  val SymTpe = typeOf[scala.Symbol]
  val atatTpe = typeOf[tag.@@[_,_]].typeConstructor

  def mkUnionNamedImpl[U <: Coproduct : WeakTypeTag](method: Tree)(elems: Tree*): Tree = {
    def mkSingletonSymbolType(c: Constant): Type =
      appliedType(atatTpe, List(SymTpe, constantType(c)))

    def mkFieldTpe(keyTpe: Type, valueTpe: Type): Type =
      appliedType(fieldTypeTpe, List(keyTpe, valueTpe.widen))

    def mkElem(keyTpe: Type, value: Tree): Tree =
      q"$value.asInstanceOf[${mkFieldTpe(keyTpe, value.tpe)}]"

    def promoteElem(elem: Tree): Tree = elem match {
      case q""" $prefix(${Literal(k: Constant)}, $v) """ => mkElem(mkSingletonSymbolType(k), v)
      case _ =>
        c.abort(c.enclosingPosition, s"$elem has the wrong shape for a record field")
    }

    val q"${methodString: String}" = (method: @unchecked)
    if(methodString != "apply")
      c.abort(c.enclosingPosition, s"this method must be called as 'apply' not '$methodString'")

    val elem =
      elems match {
        case Seq(e) => e
        case _ =>
          c.abort(c.enclosingPosition, s"only one branch of a union may be inhabited")
      }

    q""" _root_.shapeless.Coproduct[${weakTypeOf[U]}](${promoteElem(elem)}) """
  }
}
