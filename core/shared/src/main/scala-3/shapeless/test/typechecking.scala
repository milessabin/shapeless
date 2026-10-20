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
package test

import java.util.regex.Pattern

import scala.compiletime.testing.{Error, typeCheckErrors}
import scala.quoted.*

/**
 * A utility which ensures that a code fragment does not typecheck.
 *
 * On Scala 3 the error messages are those of the Scala 3 compiler. To allow expected error patterns to be shared
 * between Scala versions, an expected pattern is also matched against Scala 2 style renderings of the most common
 * errors ("could not find implicit value for parameter ..." and "type mismatch; ...").
 */
object illTyped {
  transparent inline def apply(inline code: String): Unit =
    ${ IllTypedMacros.applyImpl('{ typeCheckErrors(wrapped(code)) }, '{ None }) }

  transparent inline def apply(inline code: String, inline expected: String): Unit =
    ${ IllTypedMacros.applyImpl('{ typeCheckErrors(wrapped(code)) }, '{ Some(expected) }) }

  /** The code wrapped in a val in an object, as on Scala 2 (so that eg. `cachedImplicit` sees an enclosing val). */
  inline def wrapped(inline code: String): String = "object illTypedDummy0 { val illTypedDummy1 = { " + code + " } }"
}

object IllTypedMacros {
  def applyImpl(errorsExpr: Expr[List[Error]], expectedExpr: Expr[Option[String]])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val errors = typeCheckErrorMessages(errorsExpr)

    val expected = expectedExpr.asTerm.underlyingArgument match {
      case Apply(_, List(arg)) => arg.underlyingArgument match {
        case Literal(StringConstant(s)) => Some(s)
        case other => report.errorAndAbort(s"Expected error pattern must be a literal string: ${other.show}")
      }
      case _ => None
    }
    val expMsg = expected.fold("Expected some error.")(s => "Expected error matching: " + s)

    errors.find(_._2).foreach { case (msg, _) => report.error(s"Parsing failed.\n$msg") }

    if (errors.isEmpty) report.error("Type-checking succeeded unexpectedly.\n" + expMsg)
    else expected.foreach { exp =>
      val expPat = Pattern.compile(exp, Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
      if (!errors.exists { case (msg, _) => (msg :: scala2Renderings(msg)).exists(m => expPat.matcher(m).matches) })
        report.error("Type-checking failed in an unexpected way.\n" + expMsg + "\nActual error: " + errors.head._1)
    }

    '{ () }
  }

  /** Scala 3 appends suggestions of imports to some error messages; they are not part of the error proper. */
  def withoutImportSuggestions(msg: String): String =
    msg.replaceAll("(?s)\\n+\\s*(One of the following imports|The following import).*$", "")

  /** Scala 2 style renderings of a Scala 3 error message, for the most common kinds of error. */
  def scala2Renderings(msg: String): List[String] = {
    val noGiven = """(?s)No given instance of type (.*?) was found for (?:parameter (\S+)|a context parameter) of .*""".r
    val noImplicit = """(?s)No implicit (?:value|view) of type (.*?) (?:was found|available).*""".r
    val mismatch = """(?s)\s*Found:\s*(.*?)\n\s*Required:\s*(.*?)(\n.*)?""".r
    msg match {
      case noGiven(tpe, param) =>
        List(s"could not find implicit value for parameter ${Option(param).getOrElse("evidence")}: $tpe")
      case noImplicit(tpe) =>
        List(s"could not find implicit value for parameter evidence: $tpe")
      case mismatch(found, required, _) =>
        List(s"type mismatch;\n found   : ${found.trim}\n required: ${required.trim}")
      case _ => Nil
    }
  }

  /** The messages of the errors in an expansion of `typeCheckErrors`, with whether each is a parser error. */
  def typeCheckErrorMessages(errorsExpr: Expr[List[Error]])(using Quotes): List[(String, Boolean)] = {
    import quotes.reflect.*
    val errorSym = TypeRepr.of[Error].typeSymbol
    new TreeAccumulator[List[(String, Boolean)]] {
      def foldTree(acc: List[(String, Boolean)], tree: Tree)(owner: Symbol): List[(String, Boolean)] = tree match {
        case Apply(fn, List(msg, _, _, kind)) if fn.symbol.owner == errorSym.companionModule.moduleClass || fn.symbol.owner == errorSym =>
          msg match {
            case Literal(StringConstant(m)) => (withoutImportSuggestions(m), kind.symbol.name == "Parser") :: acc
            case _ => foldOverTree(acc, tree)(owner)
          }
        case _ => foldOverTree(acc, tree)(owner)
      }
    }.foldTree(Nil, errorsExpr.asTerm)(Symbol.spliceOwner).reverse
  }
}
