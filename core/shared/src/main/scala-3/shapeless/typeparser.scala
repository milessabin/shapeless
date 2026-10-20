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

import scala.quoted.*

/**
 * Support for the Scala 2 "type provider" idiom (eg. ``Witness.`"foo"`.T``, ``HList.`Int, String`.T``) on Scala 3.
 *
 * A `selectDynamic` call is expanded to a value whose type is the intersection of a stable singleton type (so that the
 * expression is accepted as a path in type position) and a refinement carrying the requested type members.
 *
 * Type strings are typechecked at the call site, as on Scala 2, by the compiler (see `CompilerShim.typeCheck`). This
 * depends on compiler internals. With a compiler for which shapeless has no `CompilerShim`, they are interpreted by a
 * small parser here instead, which supports literal types, tuples, applied types and `.type` singletons. Names are
 * then resolved against the enclosing classes and objects of the call site, then the `shapeless`, `shapeless.nat`,
 * `scala`, `scala.Predef` and `java.lang` scopes, and finally as fully qualified names, so names brought into scope by
 * imports or defined locally within a block are not visible.
 */
object TypeCarrier {
  /** A stable path used as the singleton part of type carriers. Not intended for direct use. */
  val stable: Any = ()
}

object TypeParser {
  def commaSeparated(str: String): List[String] = {
    val (parts, last, _) = str.foldLeft((List.empty[String], new StringBuilder, 0)) {
      case ((acc, cur, depth), c) => c match {
        case ',' if depth == 0 => (cur.toString.trim :: acc, new StringBuilder, depth)
        case '(' | '[' => (acc, cur += c, depth + 1)
        case ')' | ']' => (acc, cur += c, (depth - 1) max 0)
        case _ => (acc, cur += c, depth)
      }
    }
    val all = (last.toString.trim :: parts).reverse
    if (all == List("")) Nil else all
  }

  /** Parses a literal, yielding its constant type and a term denoting its value. */
  def parseLiteral(str: String)(using Quotes): Option[quotes.reflect.Literal] = {
    import quotes.reflect.*
    val s = str.trim
    val const: Option[Constant] =
      if (s.length >= 2 && s.startsWith("\"") && s.endsWith("\"")) Some(StringConstant(unescape(s.substring(1, s.length - 1))))
      else if (s.length >= 3 && s.startsWith("'") && s.endsWith("'")) unescape(s.substring(1, s.length - 1)) match {
        case c if c.length == 1 => Some(CharConstant(c.charAt(0)))
        case _ => None
      }
      else if (s == "true") Some(BooleanConstant(true))
      else if (s == "false") Some(BooleanConstant(false))
      else if (s.matches("-?[0-9]+[lL]")) scala.util.Try(LongConstant(s.init.toLong)).toOption
      else if (s.matches("-?[0-9]+")) scala.util.Try(IntConstant(s.toInt)).toOption
      else if (s.matches("-?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?[fF]")) scala.util.Try(FloatConstant(s.init.toFloat)).toOption
      else if (s.matches("-?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?[dD]?")) scala.util.Try(DoubleConstant(s.stripSuffix("d").stripSuffix("D").toDouble)).toOption
      else None
    const.map(Literal(_))
  }

  def unescape(s: String): String =
    s.replace("\\\"", "\"").replace("\\'", "'").replace("\\n", "\n").replace("\\t", "\t").replace("\\\\", "\\")

  def parseLiteralTypeOrAbort(str: String)(using Quotes): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    parseLiteral(str).map(lit => ConstantType(lit.constant)).getOrElse(report.errorAndAbort(s"Malformed literal type $str"))
  }

  /** Typechecks a type at the call site, or with no `CompilerShim`, parses it as `parseType` does. */
  def parseTypeOrAbort(str: String)(using Quotes): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    CompilerShims.optionally(_.typeCheck(str, isType = true)) match {
      case Some(typed) => typed.fold(report.errorAndAbort(_), identity)
      case None => parseType(str).getOrElse(report.errorAndAbort(s"Malformed literal or standard type $str"))
    }
  }

  /** Parses a literal type or a (possibly applied) named type. */
  def parseType(str: String)(using Quotes): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val s = str.trim
    parseLiteral(s).map(lit => ConstantType(lit.constant)).orElse {
      if (s.startsWith("(") && s.endsWith(")")) {
        val elems = commaSeparated(s.substring(1, s.length - 1))
        if (elems.size == 1) parseType(elems.head)
        else elems.foldRight(Option(List.empty[TypeRepr])) { (e, acc) =>
          acc.flatMap(ts => parseType(e).map(_ :: ts))
        }.flatMap(ts => tupleType(ts))
      } else {
        val bracket = s.indexOf('[')
        if (bracket > 0 && s.endsWith("]")) {
          val args = commaSeparated(s.substring(bracket + 1, s.length - 1))
          for {
            tycon <- resolveTypePath(s.substring(0, bracket).trim)
            targs <- args.foldRight(Option(List.empty[TypeRepr]))((e, acc) => acc.flatMap(ts => parseType(e).map(_ :: ts)))
          } yield tycon.appliedTo(targs)
        } else if (s.endsWith(".type")) resolveTermPath(s.stripSuffix(".type").trim)
        else resolveTypePath(s)
      }
    }
  }

  def tupleType(using Quotes)(elems: List[quotes.reflect.TypeRepr]): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    if (elems.isEmpty) Some(TypeRepr.of[Unit])
    else scala.util.Try(defn.TupleClass(elems.size).typeRef.appliedTo(elems)).toOption
  }

  /** The type of a term path, eg. `Foo.bar` or `foo`, as a singleton type. */
  def resolveTermPath(path: String)(using Quotes): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val segments = path.split('.').toList.map(_.trim)
    for {
      first <- lookupTerm(segments.head)
      res <- segments.tail.foldLeft(Option(first)) { (acc, seg) => acc.flatMap(selectTerm(_, seg)) }
    } yield res
  }

  def resolveTypePath(path: String)(using Quotes): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val segments = path.split('.').toList.map(_.trim)
    segments match {
      case List(name) => lookupType(name)
      case _ =>
        val (prefix, name) = (segments.init, segments.last)
        val viaTerm = for {
          first <- lookupTerm(prefix.head)
          pre <- prefix.tail.foldLeft(Option(first))((acc, seg) => acc.flatMap(selectTerm(_, seg)))
          tpe <- selectType(pre, name)
        } yield tpe
        viaTerm.orElse(scala.util.Try(Symbol.requiredClass(path)).toOption.filter(_.exists).map(_.typeRef))
    }
  }

  def selectTerm(using Quotes)(pre: quotes.reflect.TypeRepr, name: String): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val sym = pre.termSymbol match {
      case s if s.exists && s.flags.is(Flags.Package) =>
        s.declaredFields.find(_.name == name).orElse(Option(s.fieldMember(name)).filter(_.exists))
      case _ =>
        Option(pre.typeSymbol.fieldMember(name)).filter(_.exists)
          .orElse(pre.typeSymbol.methodMember(name).find(_.paramSymss.isEmpty))
    }
    sym.map(s => pre.select(s))
  }

  def selectType(using Quotes)(pre: quotes.reflect.TypeRepr, name: String): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val sym = pre.termSymbol match {
      case s if s.exists && s.flags.is(Flags.Package) =>
        s.declaredType(name).headOption.orElse(Option(s.typeMember(name)).filter(_.exists))
      case _ =>
        Option(pre.typeSymbol.typeMember(name)).filter(_.exists)
    }
    sym.map(s => pre.select(s))
  }

  /** Enclosing classes and objects of the call site, innermost first, as types usable as prefixes. */
  def enclosingPrefixes(using Quotes): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    def loop(s: Symbol, acc: List[TypeRepr]): List[TypeRepr] =
      if (s.isNoSymbol) acc.reverse
      else if (s.isPackageDef) loop(s.owner, s.termRef :: acc)
      else if (s.isClassDef) loop(s.owner, This(s).tpe :: acc)
      else loop(s.owner, acc)
    loop(Symbol.spliceOwner, Nil)
  }

  def defaultScopes(using Quotes): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    List(
      scala.util.Try(Symbol.requiredPackage("shapeless").termRef).toOption,
      scala.util.Try(Symbol.requiredModule("shapeless.nat").termRef).toOption,
      scala.util.Try(Symbol.requiredModule("shapeless.package").termRef).toOption,
      scala.util.Try(Symbol.requiredPackage("scala").termRef).toOption,
      scala.util.Try(Symbol.requiredModule("scala.Predef").termRef).toOption,
      scala.util.Try(Symbol.requiredModule("scala.package").termRef).toOption,
      scala.util.Try(Symbol.requiredPackage("java.lang").termRef).toOption
    ).flatten
  }

  def lookupType(name: String)(using Quotes): Option[quotes.reflect.TypeRepr] =
    (enclosingPrefixes ++ defaultScopes).iterator.flatMap(pre => selectType(pre, name)).nextOption()

  def lookupTerm(name: String)(using Quotes): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    (enclosingPrefixes ++ defaultScopes).iterator.flatMap(pre => selectTerm(pre, name)).nextOption()
      .orElse(scala.util.Try(Symbol.requiredPackage(name)).toOption.filter(_.exists).map(_.termRef))
  }

  /**
   * A value of a type carrier: an intersection of the stable type `TypeCarrier.stable.type` with the given type, which
   * typically is a refinement carrying type members.
   */
  def carrier(using Quotes)(tpe: quotes.reflect.TypeRepr, value: Option[quotes.reflect.Term] = None): Expr[Any] = {
    import quotes.reflect.*
    val stable = TypeRepr.of[TypeCarrier.type].select(Symbol.requiredModule("shapeless.TypeCarrier").fieldMember("stable"))
    val carrierTpe = AndType(stable, tpe)
    val v = value.getOrElse('{ () }.asTerm)
    carrierTpe.asType match {
      case '[c] => '{ ${ v.asExpr }.asInstanceOf[c] }
    }
  }

  /** A refinement of `AnyRef` with the type alias members given. */
  def refinement(using Quotes)(members: List[(String, quotes.reflect.TypeRepr)]): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    members.foldLeft(TypeRepr.of[AnyRef]) { case (acc, (name, tpe)) => Refinement(acc, name, TypeBounds(tpe, tpe)) }
  }

  /** A refinement with type `T` and, for labels, the record field constructors `->>` and `Field`. */
  def fieldTypeCarrier(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    def field = TypeLambda(
      List("V"),
      _ => List(TypeBounds.empty),
      tl => TypeRepr.of[labelled.FieldType].appliedTo(List(tpe, tl.param(0)))
    )
    refinement(List("T" -> tpe, "->>" -> field, "Field" -> field))
  }
}
