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

trait HListScalaCompat {
  /**
   * Allows to specify an `HList` type with a syntax similar to `Record` and `Union`, as follows,
   *
   * {{{
   * type ISB = HList.`Int, String, Boolean`.T
   * }}}
   *
   * Literal types are allowed, so that the following is valid,
   *
   * {{{
   * type TwoTrueStr = HList.`2, true, "str"`.T
   * }}}
   *
   * On Scala 3 type names are resolved as described for [[TypeCarrier]].
   */
  transparent inline def selectDynamic(inline tpeSelector: String): Any =
    ${ ProductMacros.hlistType('tpeSelector) }
}

trait NatProductArgs extends Dynamic {
  transparent inline def applyDynamic(inline method: String)(inline args: Int*): Any =
    ${ ProductMacros.forwardNat('this, 'method, 'args) }
}

/**
 * Trait supporting mapping dynamic argument lists to HList arguments.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.method(23, "foo", true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.methodProduct(23 :: "foo" :: true)
 * }}}
 *
 * ie. the arguments are rewritten as HList elements and the application is
 * rewritten to an application of an implementing method (identified by the
 * "Product" suffix) which accepts a single HList argument.
 *
 */
trait ProductArgs extends Dynamic {
  transparent inline def applyDynamic(inline method: String)(inline args: Any*): Any =
    ${ ProductMacros.forward('this, 'method, 'args, false) }
}

/**
 * Trait supporting mapping HList arguments to argument lists, inverse of ProductArgs.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.methodProduct(23 :: "foo" :: true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.method(23, "foo", true)
 * }}}
 *
 * ie. the HList argument is used to obtain arguments for a target method
 * (the called method named minus the "Product" suffix) in sequence  and the application
 * is rewritten to an application of the target method
 *
 */
trait FromProductArgs extends Dynamic {
  transparent inline def applyDynamic(inline method: String)(inline hlist: HList): Any =
    ${ ProductMacros.forwardFromProduct('this, 'method, 'hlist) }
}

/**
 * Trait supporting mapping dynamic argument lists to singleton-typed HList arguments.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.method(23, "foo", true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.methodProduct(23.narrow :: "foo".narrow :: true.narrow)
 * }}}
 *
 * ie. the arguments are rewritten as singleton-typed HList elements and the
 * application is rewritten to an application of an implementing method (identified by the
 * "Product" suffix) which accepts a single HList argument.
 */
trait SingletonProductArgs extends Dynamic {
  transparent inline def applyDynamic(inline method: String)(inline args: Any*): Any =
    ${ ProductMacros.forward('this, 'method, 'args, true) }
}

object ProductMacros {
  def hlistType(tpeSelector: Expr[String])(using Quotes): Expr[Any] = {
    val elems = TypeParser.commaSeparated(tpeSelector.valueOrAbort).map(TypeParser.parseTypeOrAbort)
    TypeParser.carrier(TypeParser.refinement(List("T" -> MacroUtils.mkHListTpe(elems))))
  }

  def varargs(using Quotes)(args: Expr[Seq[Any]]): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    args match {
      case Varargs(es) => es.toList.map(_.asTerm.underlyingArgument)
      case _ => report.errorAndAbort("this method must be called with explicit arguments")
    }
  }

  def forward(lhs: Expr[Any], method: Expr[String], args: Expr[Seq[Any]], narrow: Boolean)(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val elems = varargs(args).map(e => if (narrow) narrowValue(e) else e)
    MacroUtils.applyInferred(lhs.asTerm, method.valueOrAbort + "Product", Nil, List(List(MacroUtils.mkHList(elems)))).asExpr
  }

  def narrowValue(using Quotes)(value: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*
    value match {
      case lit @ Literal(const) => Typed(lit, Inferred(ConstantType(const)))
      case _ => value
    }
  }

  def forwardNat(lhs: Expr[Any], method: Expr[String], args: Expr[Seq[Int]])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val ns = varargs(args).map(t => NatMacros.natLiteral(t.asExpr))
    val methodName = method.valueOrAbort + "NatProduct"
    val qual = lhs.asTerm
    val sym = qual.tpe.widen.typeSymbol.methodMember(methodName).headOption
      .getOrElse(report.errorAndAbort(s"missing method '$methodName'"))
    val typeOnly = qual.tpe.widen.memberType(sym) match {
      case pt: PolyType => pt.resType match {
        case mt: MethodType => mt.isImplicit
        case _ => true
      }
      case _ => false
    }
    if (typeOnly) {
      val tpe = MacroUtils.mkHListTpe(ns.map(NatMacros.mkNatTpe))
      MacroUtils.applyInferred(qual, methodName, List(tpe), Nil).asExpr
    } else {
      val values = ns.map(n => NatMacros.mkNatValue(n).asTerm)
      MacroUtils.applyInferred(qual, methodName, Nil, List(List(MacroUtils.mkHList(values)))).asExpr
    }
  }

  def forwardFromProduct(lhs: Expr[Any], method: Expr[String], hlist: Expr[HList])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val methodString = method.valueOrAbort
    if (!methodString.endsWith("Product")) report.errorAndAbort(s"missing method '$methodString'")
    val methodName = methodString.stripSuffix("Product")
    val qual = lhs.asTerm
    val sym = qual.tpe.widen.typeSymbol.methodMember(methodName).headOption
      .getOrElse(report.errorAndAbort(s"missing method '$methodName'"))
    val arities = sym.paramSymss.filterNot(ps => ps.exists(_.isType) || ps.exists(p => p.flags.is(Flags.Implicit) || p.flags.is(Flags.Given))).map(_.size)
    val hlTerm = hlist.asTerm.underlyingArgument
    val size = MacroUtils.unpackHList(hlTerm.tpe.widen).size
    if (size != arities.sum)
      report.errorAndAbort(s"wrong number of arguments for method '$methodName': expected ${arities.sum}, found $size")
    ValDef.let(Symbol.spliceOwner, "hlist", hlTerm) { ref =>
      val elems = List.iterate[Term](ref, arities.sum + 1)(t => Select.unique(t, "tail")).init.map(t => Select.unique(t, "head"))
      val argss = arities.foldLeft((List.empty[List[Term]], elems)) { case ((acc, rest), n) =>
        (rest.take(n) :: acc, rest.drop(n))
      }._1.reverse
      MacroUtils.applyInferred(qual, methodName, Nil, argss)
    }.asExpr
  }
}
