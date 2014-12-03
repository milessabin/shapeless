/*
 * Copyright (c) 2011-14 Miles Sabin
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

/**
 * Record operations on `HList`'s with field-like elements.
 *
 * @author Miles Sabin
 */
object record {
  import syntax.RecordOps

  implicit def recordOps[L <: HList](l : L) : RecordOps[L] = new RecordOps(l)

  /**
   * Records encoded as `HLists` of their value types intersected with the
   * singleton types of their keys.
   *
   * Record types may be written using a relatively concise syntax thanks to a trick
   * due to Denys Shabalin (@den_sh) and Eugene Burmako (@xeno_by). We use a
   * combination of `selectDynamic` and backticks to embed a type in a path which
   * appears to the compiler as stable,
   *
   * {{{
   * type Xyz = Record.`'x -> Int, 'y -> String, 'z -> Boolean`.T
   * }}}
   *
   * The use of singleton-typed `Symbols` as keys would make this type extremely
   * laborious to write out by hand.
   *
   * There is also a mechanism for creating values of record types using Scala's
   * named argument syntax. Values of the type just defined can be created as follows,
   *
   * {{{
   * val xyz = Union(x = 23, y = "foo", z = true)
   * xyz.get('y) // == Some("foo")
   * }}}
   */
  object Record extends Dynamic {
    def applyDynamicNamed(method: String)(rec: Any*): HList = macro RecordMacros.mkRecordNamedImpl

    def selectDynamic(tpeSelector: String): Any = macro LabelledMacros.recordTypeImpl
  }
}

/**
 * Trait supporting mapping named argument lists to record arguments.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.method(x = 23, y = "foo", z = true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.methodImpl('x ->> 23 :: 'y ->> "foo", 'z ->> true)
 * }}}
 *
 * ie. the named argments are rewritten as record fields with the argument name
 * encoded as a singleton-typed `Symbol` and the application is rewritten to an
 * application of an implementing method (identified by the "Impl" suffx) which
 * accepts a single record argument.
 */
trait RecordArgs extends Dynamic {
  def applyDynamic(method: String)(): Any = macro RecordMacros.forwardImpl
  def applyDynamicNamed(method: String)(rec: Any*): Any = macro RecordMacros.forwardNamedImpl
}

class RecordMacros(val c: whitebox.Context) {
  import c.universe._
  import internal.constantType
  import labelled.FieldType

  val hconsValueTree = reify {  ::  }.tree
  val hnilValueTree  = reify { HNil: HNil }.tree
  val fieldTypeTpe = typeOf[FieldType[_, _]].typeConstructor
  val SymTpe = typeOf[scala.Symbol]
  val atatTpe = typeOf[tag.@@[_,_]].typeConstructor

  def mkRecordNamedImpl(method: Tree)(rec: Tree*): Tree = {
    val q"${methodString: String}" = method
    if(methodString != "apply")
      c.abort(c.enclosingPosition, s"this method must be called as 'apply' not '$methodString'")

    mkRecordImpl(rec: _*)
  }

  def forwardImpl(method: Tree)(): Tree = forwardNamedImpl(method)()

  def forwardNamedImpl(method: Tree)(rec: Tree*): Tree = {
    val q"${methodString: String}" = method
    val methodName = TermName(methodString+"Record")
    val recTree = mkRecordImpl(rec: _*)
    val app = c.macroApplication

    val lhs = app match {
      case q"$lhs.applyDynamicNamed($_)(..$_)" => lhs
      case q"$lhs.applyDynamic($_)()" => lhs
      case other =>
        c.abort(c.enclosingPosition, s"bogus prefix '$other'")
    }

    q""" $lhs.$methodName($recTree) """
  }

  def mkRecordImpl(rec: Tree*): Tree = {
    def mkSingletonSymbolType(c: Constant): Type =
      appliedType(atatTpe, List(SymTpe, constantType(c)))

    def mkFieldTpe(keyTpe: Type, valueTpe: Type): Type =
      appliedType(fieldTypeTpe, List(keyTpe, valueTpe.widen))

    def mkElem(keyTpe: Type, value: Tree): Tree =
      q"$value.asInstanceOf[${mkFieldTpe(keyTpe, value.tpe)}]"

    def promoteElem(elem: Tree): Tree = elem match {
      case q""" (${c.universe.Literal(k: Constant)}, $v) """ => mkElem(mkSingletonSymbolType(k), v)
      case _ =>
        c.abort(c.enclosingPosition, s"$elem has the wrong shape for a record field")
    }

    rec.foldRight(hnilValueTree) {
      case(elem, acc) => q""" $hconsValueTree(${promoteElem(elem)}, $acc) """
    }
  }
}
