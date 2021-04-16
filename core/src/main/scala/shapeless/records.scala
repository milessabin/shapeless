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
   * val xyz = Record(x = 23, y = "foo", z = true)
   * xyz('y) // == "foo"
   * }}}
   */
  object Record extends Dynamic {
    def applyDynamic(method: String)(rec: Any*): HList = macro RecordMacros.mkRecordEmptyImpl
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
 * lhs.methodRecord('x ->> 23 :: 'y ->> "foo", 'z ->> true)
 * }}}
 *
 * ie. the named arguments are rewritten as record fields with the argument name
 * encoded as a singleton-typed `Symbol` and the application is rewritten to an
 * application of an implementing method (identified by the "Record" suffix) which
 * accepts a single record argument.
 */
trait RecordArgs extends Dynamic {
  def applyDynamic(method: String)(): Any = macro RecordMacros.forwardImpl
  def applyDynamicNamed(method: String)(rec: Any*): Any = macro RecordMacros.forwardNamedImpl
}

/**
 * Trait supporting mapping record arguments to named argument lists, inverse of RecordArgs.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.methodRecord('x ->> 23 :: 'y ->> "foo" :: 'z ->> true :: HNil)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.method(x = 23, y = "foo", z = true)
 * }}}
 *
 * ie. the record argument is used to look up arguments for a target method
 * (the called method named minus the "Record" suffix) by name and type and the application
 * is rewritten to an application of the target method
 */
trait FromRecordArgs extends Dynamic {
  def applyDynamic[L <: HList](method: String)(rec: L): Any = macro RecordMacros.forwardFromRecordImpl[L]
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

  def mkRecordEmptyImpl(method: Tree)(rec: Tree*): Tree = {
    if(rec.nonEmpty)
      c.abort(c.enclosingPosition, "this method must be called with named arguments")

    q"_root_.shapeless.HNil"
  }

  def mkRecordNamedImpl(method: Tree)(rec: Tree*): Tree = {
    val q"${methodString: String}" = (method: @unchecked)
    if(methodString != "apply")
      c.abort(c.enclosingPosition, s"this method must be called as 'apply' not '$methodString'")

    mkRecordImpl(rec: _*)
  }

  def forwardImpl(method: Tree)(): Tree = forwardNamedImpl(method)()

  def forwardNamedImpl(method: Tree)(rec: Tree*): Tree = {
    val lhs = c.prefix.tree 
    val lhsTpe = lhs.tpe

    val q"${methodString: String}" = (method: @unchecked)
    val methodName = TermName(methodString+"Record")

    if(lhsTpe.member(methodName) == NoSymbol)
      c.abort(c.enclosingPosition, s"missing method '$methodName'")

    val recTree = mkRecordImpl(rec: _*)

    q""" $lhs.$methodName($recTree) """
  }

  def forwardFromRecordImpl[L <: HList](method: Tree)(rec: Expr[L]): Tree = {
    val lhs = c.prefix.tree
    val lhsTpe = lhs.tpe

    val q"${methodString: String}" = (method: @unchecked)

    if (!methodString.matches(".*Record$"))
      c.abort(c.enclosingPosition, s"missing method '$methodString'")

    val methodName = TermName(methodString.replaceAll("Record$", ""))

    if(!lhsTpe.member(methodName).isMethod)
      c.abort(c.enclosingPosition, s"missing method '$methodName'")

    val params = mkParamsImpl(lhsTpe.member(methodName).asMethod, rec)
    q""" $lhs.$methodName(...$params) """
  }

  def mkSingletonSymbolType(c: Constant): Type =
    appliedType(atatTpe, List(SymTpe, constantType(c)))

  def mkFieldTpe(keyTpe: Type, valueTpe: Type): Type =
    appliedType(fieldTypeTpe, List(keyTpe, valueTpe.widen))

  def mkRecordImpl(rec: Tree*): Tree = {
    def mkElem(keyTpe: Type, value: Tree): Tree =
      q"$value.asInstanceOf[${mkFieldTpe(keyTpe, value.tpe)}]"

    def promoteElem(elem: Tree): Tree = elem match {
      case q""" $prefix(${Literal(k: Constant)}, $v) """ => mkElem(mkSingletonSymbolType(k), v)
      case _ =>
        c.abort(c.enclosingPosition, s"$elem has the wrong shape for a record field")
    }

    rec.foldRight(hnilValueTree) {
      case(elem, acc) => q""" $hconsValueTree(${promoteElem(elem)}, $acc) """
    }
  }

  def mkParamsImpl[L <: HList](method: MethodSymbol, rec: Expr[L]): List[List[Tree]] = {
    def mkElem(keyTpe: Type, value: Tree): Tree =
      q"_root_.scala.Predef.implicitly[_root_.shapeless.ops.hlist.Selector[${rec.actualType}, ${mkFieldTpe(keyTpe, value.tpe)}]].apply($rec)"

    method.paramLists.filterNot(_.forall(x => x.isImplicit)).map(_.map { x =>
      mkElem(mkSingletonSymbolType(Constant(x.name.decodedName.toString)), q"${x.typeSignature}")
    })

  }
}
