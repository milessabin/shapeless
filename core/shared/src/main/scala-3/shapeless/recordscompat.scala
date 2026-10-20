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
import scala.quoted.*

trait RecordScalaCompat {
  transparent inline def applyDynamic(inline method: String)(inline rec: Any*): HList =
    ${ RecordMacros.mkRecordEmptyImpl('method, 'rec) }

  transparent inline def applyDynamicNamed(inline method: String)(inline rec: (String, Any)*): HList =
    ${ RecordMacros.mkRecordNamedImpl('method, 'rec) }

  transparent inline def selectDynamic(inline tpeSelector: String): Any =
    ${ RecordMacros.recordType('tpeSelector) }
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
 * lhs.methodRecord("x" ->> 23 :: "y" ->> "foo", "z" ->> true)
 * }}}
 *
 * ie. the named arguments are rewritten as record fields with the argument name
 * encoded as a singleton-typed `Symbol` and the application is rewritten to an
 * application of an implementing method (identified by the "Record" suffix) which
 * accepts a single record argument.
 */
trait RecordArgs extends Dynamic {
  transparent inline def applyDynamic(inline method: String)(): Any =
    ${ RecordMacros.forwardImpl('this, 'method) }

  transparent inline def applyDynamicNamed(inline method: String)(inline rec: (String, Any)*): Any =
    ${ RecordMacros.forwardNamedImpl('this, 'method, 'rec) }
}

/**
 * Trait supporting mapping record arguments to named argument lists, inverse of RecordArgs.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.methodRecord("x" ->> 23 :: "y" ->> "foo" :: "z" ->> true :: HNil)
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
  transparent inline def applyDynamic[L <: HList](inline method: String)(inline rec: L): Any =
    ${ RecordMacros.forwardFromRecordImpl[L]('this, 'method, 'rec) }
}

object RecordMacros {
  def mkRecordEmptyImpl(method: Expr[String], rec: Expr[Seq[Any]])(using Quotes): Expr[HList] = {
    import quotes.reflect.*
    rec match {
      case Varargs(Seq()) => '{ HNil: HNil }
      case _ => report.errorAndAbort("this method must be called with named arguments")
    }
  }

  def mkRecordNamedImpl(method: Expr[String], rec: Expr[Seq[(String, Any)]])(using Quotes): Expr[HList] = {
    import quotes.reflect.*
    val methodString = method.valueOrAbort
    if (methodString != "apply") report.errorAndAbort(s"this method must be called as 'apply' not '$methodString'")
    mkRecordImpl(rec).asExprOf[HList]
  }

  def recordType(tpeSelector: Expr[String])(using Quotes): Expr[Any] =
    TypeParser.carrier(TypeParser.refinement(List("T" -> labelledType(tpeSelector.valueOrAbort, "record", MacroUtils.mkHListTpe))))

  def labelledType(using Quotes)(
    tpeString: String, variety: String, mk: List[quotes.reflect.TypeRepr] => quotes.reflect.TypeRepr
  ): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    val fields = TypeParser.commaSeparated(tpeString).map { element =>
      element.split("->") match {
        case Array(keyString, valueString) =>
          MacroUtils.mkFieldTpe(TypeParser.parseLiteralTypeOrAbort(keyString.trim), TypeParser.parseTypeOrAbort(valueString.trim))
        case _ =>
          report.errorAndAbort(s"Malformed $variety type $tpeString")
      }
    }
    mk(fields)
  }

  def forwardImpl(lhs: Expr[Any], method: Expr[String])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    MacroUtils.applyInferred(lhs.asTerm, method.valueOrAbort + "Record", Nil, List(List('{ HNil: HNil }.asTerm))).asExpr
  }

  def forwardNamedImpl(lhs: Expr[Any], method: Expr[String], rec: Expr[Seq[(String, Any)]])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    MacroUtils.applyInferred(lhs.asTerm, method.valueOrAbort + "Record", Nil, List(List(mkRecordImpl(rec)))).asExpr
  }

  def forwardFromRecordImpl[L <: HList: Type](lhs: Expr[Any], method: Expr[String], rec: Expr[L])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val methodString = method.valueOrAbort
    if (!methodString.endsWith("Record")) report.errorAndAbort(s"missing method '$methodString'")
    val methodName = methodString.stripSuffix("Record")
    val qual = lhs.asTerm
    val qualTpe = qual.tpe.widen
    val sym = qualTpe.typeSymbol.methodMember(methodName).headOption
      .getOrElse(report.errorAndAbort(s"missing method '$methodName'"))
    val recTerm = rec.asTerm.underlyingArgument
    val recTpe = recTerm.tpe.widen
    val paramss = qualTpe.memberType(sym) match {
      case mt: MethodType => explicitParams(mt)
      case _ => report.errorAndAbort(s"'$methodName' must be a non-polymorphic method")
    }
    ValDef.let(Symbol.spliceOwner, "rec", recTerm) { ref =>
      val argss = paramss.map(_.map { case (name, tpe) =>
        val fieldTpe = MacroUtils.mkFieldTpe(ConstantType(StringConstant(name)), tpe)
        val selTpe = TypeRepr.of[ops.hlist.Selector].appliedTo(List(recTpe, fieldTpe))
        Implicits.search(selTpe) match {
          case s: ImplicitSearchSuccess => Select.unique(s.tree, "apply").appliedTo(ref)
          case f: ImplicitSearchFailure => report.errorAndAbort(s"No field $name of type ${tpe.show} in record ${recTpe.show}")
        }
      })
      MacroUtils.applyInferred(qual, methodName, Nil, argss)
    }.asExpr
  }

  def explicitParams(using Quotes)(tpe: quotes.reflect.TypeRepr): List[List[(String, quotes.reflect.TypeRepr)]] = {
    import quotes.reflect.*
    tpe match {
      case mt: MethodType if !mt.isImplicit => mt.paramNames.zip(mt.paramTypes) :: explicitParams(mt.resType)
      case _ => Nil
    }
  }

  def mkRecordImpl(using Quotes)(rec: Expr[Seq[(String, Any)]]): quotes.reflect.Term = {
    import quotes.reflect.*
    val elems = rec match {
      case Varargs(es) => es.toList.map(promoteElem)
      case _ => report.errorAndAbort("this method must be called with named arguments")
    }
    MacroUtils.mkHList(elems)
  }

  def promoteElem(using Quotes)(elem: Expr[(String, Any)]): quotes.reflect.Term = {
    import quotes.reflect.*
    elem match {
      case '{ ($k: String, $v: tp) } =>
        k.asTerm.underlyingArgument match {
          case Literal(const) => mkElem(ConstantType(const), v.asTerm)
          case _ => report.errorAndAbort(s"${elem.show} has the wrong shape for a record field")
        }
      case '{ ($k: String) -> ($v: tp) } =>
        k.asTerm.underlyingArgument match {
          case Literal(const) => mkElem(ConstantType(const), v.asTerm)
          case _ => report.errorAndAbort(s"${elem.show} has the wrong shape for a record field")
        }
      case _ => report.errorAndAbort(s"${elem.show} has the wrong shape for a record field")
    }
  }

  def mkElem(using Quotes)(key: quotes.reflect.TypeRepr, value: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*
    val fieldTpe = MacroUtils.mkFieldTpe(key, value.tpe.widen)
    fieldTpe.asType match {
      case '[f] => '{ ${ value.asExpr }.asInstanceOf[f] }.asTerm
    }
  }
}
